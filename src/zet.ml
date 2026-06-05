open! Core
open! Bonsai_term
open Bonsai.Let_syntax
module Db = Db

(* ── Phase 2 skeleton: read-only two-pane browse UI ───────────────────────
   The note corpus is loaded once and passed in immutably; only the cursor and
   pane focus live in Bonsai state. Components (virtual_list, scroller) and FTS
   search come in later passes — this is the strace_ui-shaped skeleton. *)

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
    }
  [@@deriving sexp_of, equal]

  let initial = { cursor = 0; focus = List }
end

module Action = struct
  type t =
    | Cursor_up
    | Cursor_down
    | Cursor_top
    | Cursor_bottom
    | Toggle_focus
    | Focus_detail
  [@@deriving sexp_of]
end

(* Pure reducer. [count] is the corpus size, threaded in so cursor motion can
   clamp without the model needing to carry the notes. *)
let apply_action_pure ~count (model : Model.t) (action : Action.t) : Model.t =
  let last = Int.max 0 (count - 1) in
  let clamp i = Int.clamp_exn i ~min:0 ~max:last in
  match action with
  | Cursor_up -> { model with cursor = clamp (model.cursor - 1) }
  | Cursor_down -> { model with cursor = clamp (model.cursor + 1) }
  | Cursor_top -> { model with cursor = 0 }
  | Cursor_bottom -> { model with cursor = last }
  | Toggle_focus ->
    let focus : Focus.t =
      match model.focus with
      | List -> Detail
      | Detail -> List
    in
    { model with focus }
  | Focus_detail -> { model with focus = Detail }
;;

let accent = Attr.Color.Expert.cyan
let dim = Attr.Color.Expert.lightblack

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
  match notes with
  | [] -> View.center (View.text ~attrs:[ Attr.fg dim ] "(no notes)") ~within:{ width; height }
  | _ ->
    let rows =
      List.mapi notes ~f:(fun i note ->
        render_list_row ~width ~is_selected:(i = cursor) note)
    in
    View.vcat rows
;;

(* Detail pane: header (title/slug/kind/date) + body of the selected note. *)
let render_detail ~width (note : Db.Note.t option) =
  match note with
  | None -> View.text ~attrs:[ Attr.fg dim ] "(no note selected)"
  | Some note ->
    let meta =
      [ Printf.sprintf "#%d  %s" note.id note.kind ]
      @ (match note.slug with
         | Some s -> [ Printf.sprintf "slug: %s" s ]
         | None -> [])
      @ (match note.entry_date with
         | Some d -> [ Printf.sprintf "date: %s" d ]
         | None -> [])
    in
    let header =
      View.vcat
        (View.text ~attrs:[ Attr.fg accent; Attr.bold ] (Db.Note.display_title note)
         :: List.map meta ~f:(fun line -> View.text ~attrs:[ Attr.fg dim ] line))
    in
    let body_lines =
      String.split_lines note.body
      |> List.map ~f:(fun line ->
        let line =
          if String.length line > width
          then String.prefix line (Int.max 0 width)
          else line
        in
        View.text line)
    in
    View.vcat ([ header; View.text "" ] @ body_lines)
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
  let inject =
    let%arr set_model in
    fun (action : Action.t) ->
      set_model (fun model -> apply_action_pure ~count model action)
  in
  (* Border boxes cost 2 cols each (left+right); two boxes = 4. Split the rest
     golden-ratio: list ~38%, detail ~62%. *)
  let panes =
    let%arr { Dimensions.width; height } = dimensions in
    let content_width = width - 4 in
    let list_w = Int.min 50 (content_width * 382 / 1000) |> Int.max 10 in
    let detail_w = Int.max 10 (content_width - list_w) in
    let pane_h = Int.max 3 (height - 2) in
    list_w, detail_w, pane_h
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
      box ~focused:(not list_focused) ~title:"Detail" (render_detail ~width:detail_w selected)
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
      | Key_press { key = (ASCII 'j' | Arrow `Down); mods = [] } -> inject Cursor_down
      | Key_press { key = (ASCII 'k' | Arrow `Up); mods = [] } -> inject Cursor_up
      | Key_press { key = ASCII 'g'; mods = [] } -> inject Cursor_top
      | Key_press { key = ASCII 'G'; mods = [] } -> inject Cursor_bottom
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

(* Print a note list the way the headless subcommands report results: one line
   per note, tab-separated, so output is greppable and stable. *)
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

(* Synchronous entry point for the no-subcommand group body, which runs outside
   the Async scheduler and must return [unit]. Boots the scheduler, runs the
   TUI, and raises on error. *)
let launch_tui_blocking db_path =
  match Async.Thread_safe.block_on_async (fun () -> launch_tui db_path) with
  | Ok (Ok ()) -> ()
  | Ok (Error e) -> Error.raise e
  | Error exn -> raise exn
;;

(* The interactive browser. Mirrors what bare `zet` does, but lets you pass -db.
   Bare `zet` (no subcommand) runs this with the default path; see [command]. *)
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

(* Headless mirror of opening a note in the detail pane: print its full body.
   IDENT is a numeric id or a slug; numeric strings are tried as id first. *)
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

(* Headless mirror of [Db.search] — full-text search printed to stdout.
   Every TUI capability gets a subcommand like this so the CLI is a complete
   surface, scriptable without the terminal UI. *)
let search_command =
  Command.basic
    ~summary:"full-text search notes (headless mirror of the TUI search)"
    ~readme:(fun () ->
      "QUERY is a raw FTS5 MATCH expression (e.g. 'ocaml', 'type NEAR system').\n\
       Prints matching notes, best-ranked first, as: id<TAB>slug<TAB>kind<TAB>title.")
    (let%map_open.Command db_path = db_path_flag
     and kind =
       flag "-kind" (optional string) ~doc:"KIND restrict to journal|note|inbox"
     and limit =
       flag "-limit" (optional_with_default 50 int) ~doc:"N max results (default: 50)"
     and query = anon ("QUERY" %: string) in
     fun () ->
       let notes = Db.with_db db_path ~f:(fun db -> Db.search db ~query ?kind ~limit ()) in
       print_notes notes)
;;

(* Top-level: bare `zet` launches the TUI (default db); subcommands are the
   headless mirrors. New features add a peer subcommand here. *)
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
