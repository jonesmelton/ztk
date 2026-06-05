open! Core
open! Bonsai_term
open Bonsai.Let_syntax
module Db = Db

(* ── Phase 2 skeleton: read-only two-pane browse UI ─────────────────────── The note
   corpus is loaded once and passed in immutably; only the cursor and pane focus live in
   Bonsai state. Components (virtual_list, scroller) and FTS search come in later passes —
   this is the strace_ui-shaped skeleton. *)

module Focus = struct
  type t =
    | List
    | Detail
  [@@deriving sexp_of, equal]
end

module Model = struct
  type t =
    { cursor : int
    ; focus : Focus.t
    ; detail_scroll : int (* top line offset of the detail body, in wrapped lines *)
    }
  [@@deriving sexp_of, equal]

  let initial = { cursor = 0; focus = List; detail_scroll = 0 }
end

module Action = struct
  (* [Up]/[Down]/[Top]/[Bottom] are routed by pane focus: they move the list cursor when
     the list is focused, and scroll the detail body when the detail pane is focused. *)
  type t =
    | Up
    | Down
    | Top
    | Bottom
    | Toggle_focus
    | Focus_detail
  [@@deriving sexp_of]
end

(* Pure reducer. [count] is the corpus size and [detail_max] the largest valid detail
   scroll offset for the selected note; both are threaded in so motion can clamp without
   the model carrying the notes or their wrapped geometry. Moving the list cursor resets
   the detail scroll, since a new note's body starts at the top. *)
let apply_action_pure ~count ~detail_max (model : Model.t) (action : Action.t) : Model.t =
  let last = Int.max 0 (count - 1) in
  let clamp_cursor i = Int.clamp_exn i ~min:0 ~max:last in
  let clamp_scroll i = Int.clamp_exn i ~min:0 ~max:(Int.max 0 detail_max) in
  let move_cursor c = { model with cursor = clamp_cursor c; detail_scroll = 0 } in
  match model.focus, action with
  | Detail, Up -> { model with detail_scroll = clamp_scroll (model.detail_scroll - 1) }
  | Detail, Down -> { model with detail_scroll = clamp_scroll (model.detail_scroll + 1) }
  | Detail, Top -> { model with detail_scroll = 0 }
  | Detail, Bottom -> { model with detail_scroll = clamp_scroll detail_max }
  | List, Up -> move_cursor (model.cursor - 1)
  | List, Down -> move_cursor (model.cursor + 1)
  | List, Top -> move_cursor 0
  | List, Bottom -> move_cursor last
  | _, Toggle_focus ->
    let focus : Focus.t =
      match model.focus with
      | List -> Detail
      | Detail -> List
    in
    { model with focus }
  | _, Focus_detail -> { model with focus = Detail }
;;

let accent = Attr.Color.Expert.cyan
let dim = Attr.Color.Expert.lightblack

(* Pin a view to exactly [width] x [height] by backing it with a transparent rectangle of
   that size, so a pane's box frame sizes to the pane — not to the longest line of
   content. Content wider/taller than the box is cropped (callers wrap/slice to avoid
   that). *)
let fit ~width ~height view =
  View.zcat [ view; View.transparent_rectangle ~width ~height ]
;;

(* Greedy word-wrap to [width] columns. Words longer than [width] are hard-split so a
   single long token can't overflow the pane. A blank input line stays one blank line. *)
let wrap_line ~width line =
  let width = Int.max 1 width in
  if String.length line <= width
  then [ line ]
  else (
    let words = String.split line ~on:' ' in
    let flush rev_lines cur =
      if String.is_empty cur then rev_lines else cur :: rev_lines
    in
    let rec hard_split word acc =
      if String.length word <= width
      then List.rev (word :: acc)
      else hard_split (String.drop_prefix word width) (String.prefix word width :: acc)
    in
    let rev_lines, cur =
      List.fold words ~init:([], "") ~f:(fun (rev_lines, cur) word ->
        if String.length word > width
        then (
          (* emit the in-progress line, then the word's full-width chunks; the final chunk
             becomes the new in-progress line. *)
          let chunks = hard_split word [] in
          let init = List.drop_last_exn chunks in
          let last = List.last_exn chunks in
          List.fold init ~init:(flush rev_lines cur) ~f:(fun acc c -> c :: acc), last)
        else if String.is_empty cur
        then rev_lines, word
        else if String.length cur + 1 + String.length word <= width
        then rev_lines, cur ^ " " ^ word
        else cur :: rev_lines, word)
    in
    List.rev (flush rev_lines cur))
;;

(* The detail body as a flat list of display lines: each source line word-wrapped to
   [width], blank lines preserved. Shared by the renderer and the scroll-clamp so the max
   scroll offset always matches what's actually drawn. *)
let detail_body_lines ~width body =
  String.split_lines body |> List.concat_map ~f:(wrap_line ~width)
;;

(* One row in the list pane: cursor-highlighted note label. *)
let render_list_row ~width ~is_selected (note : Db.Note.t) =
  let marker = if is_selected then "> " else "  " in
  let label = Printf.sprintf "%s%s" marker (Db.Note.display_title note) in
  let label =
    if String.length label > width then String.prefix label (Int.max 0 width) else label
  in
  let attrs = if is_selected then [ Attr.fg accent; Attr.bold ] else [] in
  View.text ~attrs label
;;

let render_list ~width ~height ~cursor (notes : Db.Note.t list) =
  let content =
    match notes with
    | [] ->
      View.center
        (View.text ~attrs:[ Attr.fg dim ] "(no notes)")
        ~within:{ width; height }
    | _ ->
      let count = List.length notes in
      (* Scroll offset: keep cursor in the middle of the viewport when possible. *)
      let scroll_off = Int.max 0 (cursor - (height / 2)) in
      let scroll_off = Int.min scroll_off (Int.max 0 (count - height)) in
      let visible =
        List.sub notes ~pos:scroll_off ~len:(Int.min height (count - scroll_off))
      in
      let rows =
        List.mapi visible ~f:(fun i note ->
          render_list_row ~width ~is_selected:(scroll_off + i = cursor) note)
      in
      View.vcat rows
  in
  (* Pin to the pane geometry so the box never resizes to the longest visible title. *)
  fit ~width ~height content
;;

(* Number of body lines visible at once given the pane height and the fixed-height header.
   [header_lines] is computed once and shared with the renderer. *)
let detail_header_lines (note : Db.Note.t) =
  1 (* title *)
  + 1 (* "#id  kind" *)
  + (if Option.is_some note.slug then 1 else 0)
  + (if Option.is_some note.entry_date then 1 else 0)
  + 1 (* blank separator *)
;;

(* Largest valid [detail_scroll] for [note] at the given pane geometry: the wrapped body
   line count minus the visible body rows, floored at 0. Shared with the reducer so a
   scroll offset can never point past the end of the body. *)
let detail_max_scroll ~width ~height (note : Db.Note.t option) =
  match note with
  | None -> 0
  | Some note ->
    let body = detail_body_lines ~width note.body in
    let body_rows = Int.max 0 (height - detail_header_lines note) in
    Int.max 0 (List.length body - body_rows)
;;

(* Detail pane: fixed header (title/slug/kind/date) + scrollable, word-wrapped body. The
   body is sliced by [scroll] (a wrapped-line offset) to the rows left under the header. *)
let render_detail ~width ~height ~scroll (note : Db.Note.t option) =
  let content =
    match note with
    | None -> View.text ~attrs:[ Attr.fg dim ] "(no note selected)"
    | Some note ->
      let meta =
        [ Printf.sprintf "#%d  %s" note.id note.kind ]
        @ (match note.slug with
           | Some s -> [ Printf.sprintf "slug: %s" s ]
           | None -> [])
        @
        match note.entry_date with
        | Some d -> [ Printf.sprintf "date: %s" d ]
        | None -> []
      in
      let header =
        View.vcat
          (View.text ~attrs:[ Attr.fg accent; Attr.bold ] (Db.Note.display_title note)
           :: List.map meta ~f:(fun line -> View.text ~attrs:[ Attr.fg dim ] line))
      in
      let all_body = detail_body_lines ~width note.body in
      let body_rows = Int.max 0 (height - detail_header_lines note) in
      let total = List.length all_body in
      let scroll = Int.clamp_exn scroll ~min:0 ~max:(Int.max 0 (total - body_rows)) in
      let visible =
        List.sub
          all_body
          ~pos:scroll
          ~len:(Int.min body_rows (Int.max 0 (total - scroll)))
      in
      View.vcat
        (header :: View.text "" :: List.map visible ~f:(fun line -> View.text line))
  in
  fit ~width ~height content
;;

let app ~(notes : Db.Note.t list) ~(dimensions : Dimensions.t Bonsai.t) (local_ graph)
  : view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  =
  let count = List.length notes in
  let model, set_model =
    Bonsai.state'
      Model.initial
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      graph
  in
  (* Border boxes cost 2 cols each (left+right); two boxes = 4. Split the rest
     golden-ratio: list ~38%, detail ~62%. *)
  let panes =
    let%arr { Dimensions.width; height } = dimensions in
    let content_width = width - 4 in
    let list_w = content_width * 382 / 1000 |> Int.max 10 in
    let detail_w = Int.max 10 (content_width - list_w) in
    let pane_h = Int.max 3 (height - 2) in
    list_w, detail_w, pane_h
  in
  (* Inject derives [detail_max] from the live pane geometry and the note the cursor is
     on, so the reducer can clamp detail scrolling against the actually-rendered body. *)
  let inject =
    let%arr set_model
    and _, detail_w, pane_h = panes in
    fun (action : Action.t) ->
      set_model (fun model ->
        let selected = List.nth notes model.cursor in
        let detail_max = detail_max_scroll ~width:detail_w ~height:pane_h selected in
        apply_action_pure ~count ~detail_max model action)
  in
  let view =
    let%arr model
    and { Dimensions.width; height } = dimensions
    and list_w, detail_w, pane_h = panes in
    let selected = List.nth notes model.cursor in
    let list_focused = [%equal: Focus.t] model.focus List in
    let box ~focused ~title content =
      let color = if focused then accent else dim in
      let tab = if focused then "" else " <tab>" in
      Bonsai_term_border_box.view
        ~line_type:Round_corners
        ~attrs:[ Attr.fg color ]
        ~title:[%string "%{title}%{tab}"]
        ~title_attrs:[ Attr.fg color; Attr.bold ]
        content
    in
    let list_box =
      box
        ~focused:list_focused
        ~title:"Notes"
        (render_list ~width:list_w ~height:pane_h ~cursor:model.cursor notes)
    in
    let detail_box =
      box
        ~focused:(not list_focused)
        ~title:"Detail"
        (render_detail
           ~width:detail_w
           ~height:pane_h
           ~scroll:model.detail_scroll
           selected)
    in
    let content = View.hcat [ list_box; detail_box ] in
    (* Backdrop so the framed panes sit on a full-screen rectangle. *)
    View.zcat [ content; View.rectangle ~width ~height () ]
  in
  let handler =
    let%arr inject in
    fun (event : Event.t) ->
      match event with
      | Key_press { key = Tab; mods = _ } -> inject Toggle_focus
      | Key_press { key = ASCII 'N'; mods = [ Ctrl ] }
      | Key_press { key = Arrow `Down; mods = [] } -> inject Down
      | Key_press { key = ASCII 'P'; mods = [ Ctrl ] }
      | Key_press { key = Arrow `Up; mods = [] } -> inject Up
      | Key_press { key = ASCII 'A'; mods = [ Ctrl ] } -> inject Top
      | Key_press { key = ASCII 'E'; mods = [ Ctrl ] } -> inject Bottom
      | Key_press { key = Enter; mods = [] } -> inject Focus_detail
      | _ -> Effect.Ignore
  in
  ~view, ~handler
;;

let default_db_path =
  match Sys.getenv "ZET_DB" with
  | Some path -> path
  | None ->
    (match Sys.getenv "HOME" with
     | Some home -> home ^/ ".zet/zet.db"
     | None -> "zet.db")
;;

let db_path_flag =
  let%map_open.Command db_path =
    flag
      "-db"
      (optional_with_default default_db_path string)
      ~doc:
        (sprintf
           "PATH path to the zettelkasten SQLite file (default: %s; or $ZET_DB)"
           default_db_path)
  in
  db_path
;;

(* Print a note list the way the headless subcommands report results: one line per note,
   tab-separated, so output is greppable and stable. *)
let print_notes (notes : Db.Note.t list) =
  List.iter notes ~f:(fun n ->
    print_endline
      (String.concat
         ~sep:"\t"
         [ Int.to_string n.id
         ; Option.value n.slug ~default:""
         ; n.kind
         ; Db.Note.display_title n
         ]))
;;

let launch_tui db_path =
  let notes = Db.with_db db_path ~f:Db.list_all in
  Bonsai_term.start (app ~notes)
;;

(* Synchronous entry point for the no-subcommand group body, which runs outside the Async
   scheduler and must return [unit]. Boots the scheduler, runs the TUI, and raises on
   error. *)
let launch_tui_blocking db_path =
  match Async.Thread_safe.block_on_async (fun () -> launch_tui db_path) with
  | Ok (Ok ()) -> ()
  | Ok (Error e) -> Error.raise e
  | Error exn -> raise exn
;;

(* The interactive browser. Mirrors what bare `zet` does, but lets you pass -db. Bare
   `zet` (no subcommand) runs this with the default path; see [command]. *)
let tui_command =
  Async.Command.async_or_error
    ~summary:"launch the interactive TUI browser"
    (let%map_open.Command db_path = db_path_flag in
     fun () -> launch_tui db_path)
;;

(* Headless mirror of the list pane: [Db.list_all] or [Db.list_recent]. *)
let list_command =
  Command.basic
    ~summary:"list notes (headless mirror of the TUI list pane)"
    ~readme:(fun () ->
      "Prints notes as: id<TAB>slug<TAB>kind<TAB>title. With -recent N, shows the\n\
       N most recent by entry date; otherwise all notes ordered by id.")
    (let%map_open.Command db_path = db_path_flag
     and recent =
       flag "-recent" (optional int) ~doc:"N show the N most recent notes instead of all"
     in
     fun () ->
       let notes =
         Db.with_db db_path ~f:(fun db ->
           match recent with
           | Some limit -> Db.list_recent db ~limit
           | None -> Db.list_all db)
       in
       print_notes notes)
;;

(* Headless mirror of opening a note in the detail pane: print its full body. IDENT is a
   numeric id or a slug; numeric strings are tried as id first. *)
let show_command =
  Command.basic
    ~summary:"print a single note's body (headless mirror of the TUI detail pane)"
    ~readme:(fun () ->
      "IDENT is a note id (integer) or a slug. Prints a metadata header followed\n\
       by the note body. Exits nonzero if no note matches.")
    (let%map_open.Command db_path = db_path_flag
     and ident = anon ("IDENT" %: string) in
     fun () ->
       let note =
         Db.with_db db_path ~f:(fun db ->
           match Int.of_string_opt ident with
           | Some id ->
             (match Db.get_by_id db id with
              | Some _ as n -> n
              | None -> Db.get_by_slug db ident)
           | None -> Db.get_by_slug db ident)
       in
       match note with
       | None ->
         prerr_endline (sprintf "no note matching %S" ident);
         exit 1
       | Some n ->
         printf "# %s\n" (Db.Note.display_title n);
         printf "id: %d\tkind: %s" n.id n.kind;
         Option.iter n.slug ~f:(printf "\tslug: %s");
         Option.iter n.entry_date ~f:(printf "\tdate: %s");
         Out_channel.newline stdout;
         Out_channel.newline stdout;
         print_string n.body;
         if not (String.is_suffix n.body ~suffix:"\n") then Out_channel.newline stdout)
;;

(* Headless mirror of [Db.search] — full-text search printed to stdout. Every TUI
   capability gets a subcommand like this so the CLI is a complete surface, scriptable
   without the terminal UI. *)
let search_command =
  Command.basic
    ~summary:"full-text search notes (headless mirror of the TUI search)"
    ~readme:(fun () ->
      "QUERY is a raw FTS5 MATCH expression (e.g. 'ocaml', 'type NEAR system').\n\
       Prints matching notes, best-ranked first, as: id<TAB>slug<TAB>kind<TAB>title.")
    (let%map_open.Command db_path = db_path_flag
     and kind = flag "-kind" (optional string) ~doc:"KIND restrict to journal|note|inbox"
     and limit =
       flag "-limit" (optional_with_default 50 int) ~doc:"N max results (default: 50)"
     and query = anon ("QUERY" %: string) in
     fun () ->
       let notes =
         Db.with_db db_path ~f:(fun db -> Db.search db ~query ?kind ~limit ())
       in
       print_notes notes)
;;

(* Top-level: bare `zet` launches the TUI (default db); subcommands are the headless
   mirrors. New features add a peer subcommand here. *)
let command =
  Command.group
    ~summary:{|zet — zettelkasten TUI + headless CLI|}
    ~readme:(fun () ->
      "A read-mostly terminal UI over a personal zettelkasten (SQLite), with a\n\
       headless subcommand for every TUI capability. Run `zet` with no\n\
       subcommand to launch the browser on the default database.")
    ~body:(fun ~path:_ -> launch_tui_blocking default_db_path)
    [ "tui", tui_command
    ; "list", list_command
    ; "show", show_command
    ; "search", search_command
    ]
;;
