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

(* Turns arbitrary live keystrokes into a guaranteed-valid FTS5 MATCH expression, so a
   half-typed query (a lone quote, a trailing operator, a bare '-') can never raise an
   FTS5 syntax error mid-search. Each whitespace-separated token is stripped of
   FTS5-special characters, then re-emitted as a quoted phrase with a trailing [*] for
   prefix matching — so "oc ca" becomes [ "oc"* "ca"* ]. Tokens left empty after stripping
   are dropped; an all-empty input yields [""], which the caller treats as "no query"
   (empty result set) rather than calling [Db.search]. The headless [search] subcommand
   keeps raw FTS5 syntax; this is only for the live TUI box. *)
module Fts_query = struct
  (* FTS5 treats these as syntax: quotes, parens, the prefix star, column filter, the NOT
     operator, and the NEAR caret. Strip them so a token is always an inert bareword. *)
  let is_special = function
    | '"' | '(' | ')' | '*' | ':' | '-' | '^' -> true
    | _ -> false
  ;;

  (* The cleaned, non-empty query words — the exact basis the sanitizer turns into prefix
     phrases. Highlighting reuses these so what's emphasized matches what was searched. *)
  let tokens raw =
    raw
    |> String.split_on_chars ~on:[ ' '; '\t'; '\n'; '\r' ]
    |> List.filter_map ~f:(fun token ->
      let cleaned = String.filter token ~f:(fun c -> not (is_special c)) in
      if String.is_empty cleaned then None else Some cleaned)
  ;;

  let sanitize raw =
    tokens raw
    |> List.map ~f:(fun t -> Printf.sprintf "\"%s\"*" t)
    |> String.concat ~sep:" "
  ;;
end

(* Split [line] into maximal runs of "word" vs "non-word" characters, where a word char is
   alphanumeric. Each run keeps its text; the boolean says whether it's a word. Used by
   highlighting to test whole words against query-term prefixes without splitting words. *)
let word_runs line =
  let is_word c = Char.is_alphanum c in
  String.to_list line
  |> List.group ~break:(fun a b -> Bool.( <> ) (is_word a) (is_word b))
  |> List.map ~f:(fun chars ->
    let s = String.of_char_list chars in
    ( s
    , match chars with
      | c :: _ -> is_word c
      | [] -> false ))
;;

(* Render [line] as a [View.t], emphasizing words that match the search. A word matches
   when its lowercased form starts with any of [terms] (already lowercased), mirroring the
   FTS5 prefix-* semantics the search uses. Non-matching text keeps [base] attrs; matches
   get a yellow bold overlay. With no terms this is just [View.text ~attrs:base line]. *)
let highlight ~terms ~base line =
  match terms with
  | [] -> View.text ~attrs:base line
  | _ ->
    let hit = [ Attr.fg Attr.Color.Expert.yellow; Attr.bold ] in
    word_runs line
    |> List.map ~f:(fun (text, is_word) ->
      let matched =
        is_word
        &&
        let lower = String.lowercase text in
        List.exists terms ~f:(fun t -> String.is_prefix lower ~prefix:t)
      in
      View.text ~attrs:(if matched then hit else base) text)
    |> View.hcat
;;

module Mode = struct
  type t =
    | Browse
    | Search
    | Help (* full-screen keybinding cheat-sheet overlay; dismiss back to Browse *)
  [@@deriving sexp_of, equal]
end

(* The live query line: [buf] is the raw text, [cursor] a byte offset into it (0..len).
   The buffer is the single source of truth for the query — the result set is derived from
   it, so there is no separate query state to keep in sync. *)
module Editor = struct
  type t =
    { buf : string
    ; cursor : int
    }
  [@@deriving sexp_of, equal]

  let empty = { buf = ""; cursor = 0 }
end

module Model = struct
  type t =
    { cursor : int (* selection index into the *active* list (browse corpus or results) *)
    ; focus : Focus.t
    ; detail_scroll : int (* top line offset of the detail body, in wrapped lines *)
    ; mode : Mode.t
    ; editor : Editor.t
    }
  [@@deriving sexp_of, equal]

  let initial =
    { cursor = 0; focus = List; detail_scroll = 0; mode = Browse; editor = Editor.empty }
  ;;
end

module Action = struct
  (* [Up]/[Down]/[Top]/[Bottom] are routed by pane focus: they move the list cursor when
     the list is focused, and scroll the detail body when the detail pane is focused. The
     [Search_*] / editing variants only fire while [mode = Search]; the emacs editing set
     is lifted from strace_ui's filter_editor. *)
  type t =
    | Up
    | Down
    | Top
    | Bottom
    | Toggle_focus
    | Focus_detail
    | Start_search (* [/] in Browse: enter Search, fresh empty query *)
    | Exit_search (* Esc in Search: back to Browse, clear query *)
    | Open_help (* [?] in Browse: show the keybinding overlay *)
    | Close_help (* Esc/C-g/?/q in Help: back to Browse *)
    | Insert of char
    | Backspace
    | Delete_forward
    | Move_left
    | Move_right
    | Move_to_start
    | Move_to_end
    | Kill_to_end
    | Kill_word_backward
  [@@deriving sexp_of]
end

(* Map a raw key to an action *given the current mode*. Routing lives here — not in the
   Bonsai handler — so it reads the live model inside [set_model] rather than a handler
   value that's only as fresh as the last view recompute. (A burst of keys between frames,
   e.g. typing right after [/], would otherwise be routed by a stale mode.) Returns [None]
   for keys with no binding in the current mode. *)
let route (model : Model.t) (event : Event.t) : Action.t option =
  match model.mode with
  (* Help overlay swallows the keyboard: any of Esc, C-g, ?, or q dismisses it; everything
     else is inert so a stray key can't act on the list underneath. *)
  | Help ->
    (match event with
     | Key_press { key = Escape; mods = _ }
     | Key_press { key = ASCII 'G'; mods = [ Ctrl ] }
     | Key_press { key = ASCII '?'; mods = _ }
     | Key_press { key = ASCII 'q'; mods = [] } -> Some Close_help
     | _ -> None)
  (* Search mode captures the keyboard for editing the query line. Esc leaves search;
     Enter commits — it hands focus to the list so you can navigate results, query still
     applied. The emacs editing set is lifted from strace_ui's filter_editor. *)
  | Search ->
    (match event with
     | Key_press { key = Escape; mods = _ } -> Some Exit_search
     | Key_press { key = Enter; mods = _ } -> Some Toggle_focus
     | Key_press { key = Backspace; mods = _ } -> Some Backspace
     | Key_press { key = Delete; mods = _ }
     | Key_press { key = ASCII 'D'; mods = [ Ctrl ] } -> Some Delete_forward
     | Key_press { key = ASCII 'A'; mods = [ Ctrl ] } | Key_press { key = Home; mods = _ }
       -> Some Move_to_start
     | Key_press { key = ASCII 'E'; mods = [ Ctrl ] } | Key_press { key = End; mods = _ }
       -> Some Move_to_end
     | Key_press { key = ASCII 'K'; mods = [ Ctrl ] } -> Some Kill_to_end
     | Key_press { key = ASCII 'W'; mods = [ Ctrl ] } -> Some Kill_word_backward
     | Key_press { key = ASCII 'B'; mods = [ Ctrl ] }
     | Key_press { key = Arrow `Left; mods = [] } -> Some Move_left
     | Key_press { key = ASCII 'F'; mods = [ Ctrl ] }
     | Key_press { key = Arrow `Right; mods = [] } -> Some Move_right
     | Key_press { key = ASCII 'N'; mods = [ Ctrl ] }
     | Key_press { key = Arrow `Down; mods = [] } -> Some Down
     | Key_press { key = ASCII 'P'; mods = [ Ctrl ] }
     | Key_press { key = Arrow `Up; mods = [] } -> Some Up
     | Key_press { key = ASCII c; mods = [] } -> Some (Insert c)
     | _ -> None)
  | Browse ->
    (match event with
     | Key_press { key = ASCII '/'; mods = [] } -> Some Start_search
     | Key_press { key = ASCII '?'; mods = _ } -> Some Open_help
     | Key_press { key = Tab; mods = _ } -> Some Toggle_focus
     | Key_press { key = ASCII 'N'; mods = [ Ctrl ] }
     | Key_press { key = Arrow `Down; mods = [] } -> Some Down
     | Key_press { key = ASCII 'P'; mods = [ Ctrl ] }
     | Key_press { key = Arrow `Up; mods = [] } -> Some Up
     | Key_press { key = ASCII 'A'; mods = [ Ctrl ] } -> Some Top
     | Key_press { key = ASCII 'E'; mods = [ Ctrl ] } -> Some Bottom
     | Key_press { key = Enter; mods = [] } -> Some Focus_detail
     | _ -> None)
;;

(* Pure reducer. [count] is the corpus size and [detail_max] the largest valid detail
   scroll offset for the selected note; both are threaded in so motion can clamp without
   the model carrying the notes or their wrapped geometry. Moving the list cursor resets
   the detail scroll, since a new note's body starts at the top. *)
(* Emacs-style backward word boundary from [cursor] in [buf]: skip trailing spaces, then
   skip the word, returning the offset where the word starts. Lifted from strace_ui's
   filter_editor. *)
let word_boundary_backward buf cursor =
  let is_space i = Char.equal buf.[i] ' ' in
  let i = ref cursor in
  while !i > 0 && is_space (!i - 1) do
    decr i
  done;
  while !i > 0 && not (is_space (!i - 1)) do
    decr i
  done;
  !i
;;

(* Any edit to the query buffer re-selects from the top of the (new) result set, so cursor
   and detail scroll reset to 0. The list selection index and the edit-line cursor are
   different things; only the latter changes here. *)
let edit_query (model : Model.t) ~f : Model.t =
  { model with editor = f model.editor; cursor = 0; detail_scroll = 0 }
;;

let apply_action_pure ~count ~detail_max (model : Model.t) (action : Action.t) : Model.t =
  let last = Int.max 0 (count - 1) in
  let clamp_cursor i = Int.clamp_exn i ~min:0 ~max:last in
  let clamp_scroll i = Int.clamp_exn i ~min:0 ~max:(Int.max 0 detail_max) in
  let move_cursor c = { model with cursor = clamp_cursor c; detail_scroll = 0 } in
  let clamp_edit c = Int.clamp_exn c ~min:0 ~max:(String.length model.editor.buf) in
  match action with
  | Start_search ->
    { model with mode = Search; editor = Editor.empty; cursor = 0; detail_scroll = 0 }
  | Exit_search ->
    { model with mode = Browse; editor = Editor.empty; cursor = 0; detail_scroll = 0 }
  | Open_help -> { model with mode = Help }
  | Close_help -> { model with mode = Browse }
  | Insert c ->
    edit_query model ~f:(fun { buf; cursor } ->
      let cursor = Int.clamp_exn cursor ~min:0 ~max:(String.length buf) in
      { buf = String.prefix buf cursor ^ String.of_char c ^ String.drop_prefix buf cursor
      ; cursor = cursor + 1
      })
  | Backspace ->
    edit_query model ~f:(fun { buf; cursor } ->
      if cursor > 0
      then
        { buf = String.prefix buf (cursor - 1) ^ String.drop_prefix buf cursor
        ; cursor = cursor - 1
        }
      else { buf; cursor })
  | Delete_forward ->
    edit_query model ~f:(fun { buf; cursor } ->
      if cursor < String.length buf
      then
        { buf = String.prefix buf cursor ^ String.drop_prefix buf (cursor + 1); cursor }
      else { buf; cursor })
  | Kill_to_end ->
    edit_query model ~f:(fun { buf; cursor } ->
      { buf = String.prefix buf cursor; cursor })
  | Kill_word_backward ->
    edit_query model ~f:(fun { buf; cursor } ->
      let start = word_boundary_backward buf cursor in
      { buf = String.prefix buf start ^ String.drop_prefix buf cursor; cursor = start })
  | Move_left ->
    { model with
      editor = { model.editor with cursor = clamp_edit (model.editor.cursor - 1) }
    }
  | Move_right ->
    { model with
      editor = { model.editor with cursor = clamp_edit (model.editor.cursor + 1) }
    }
  | Move_to_start -> { model with editor = { model.editor with cursor = 0 } }
  | Move_to_end ->
    { model with editor = { model.editor with cursor = String.length model.editor.buf } }
  (* Navigation is routed by pane focus: in Detail the keys scroll the body, in List they
     move the selection over the active list. *)
  | Up ->
    if Focus.equal model.focus Detail
    then { model with detail_scroll = clamp_scroll (model.detail_scroll - 1) }
    else move_cursor (model.cursor - 1)
  | Down ->
    if Focus.equal model.focus Detail
    then { model with detail_scroll = clamp_scroll (model.detail_scroll + 1) }
    else move_cursor (model.cursor + 1)
  | Top ->
    if Focus.equal model.focus Detail
    then { model with detail_scroll = 0 }
    else move_cursor 0
  | Bottom ->
    if Focus.equal model.focus Detail
    then { model with detail_scroll = clamp_scroll detail_max }
    else move_cursor last
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

(* One row in the list pane: a selection marker + the note label, with search terms
   emphasized. The label is truncated to the pane width first, so highlighting runs over
   exactly what's drawn (and the marker stays plain). *)
let render_list_row ~terms ~width ~is_selected (note : Db.Note.t) =
  let marker = if is_selected then "> " else "  " in
  let title = Db.Note.display_title note in
  let label =
    let full = marker ^ title in
    if String.length full > width then String.prefix full (Int.max 0 width) else full
  in
  let base = if is_selected then [ Attr.fg accent; Attr.bold ] else [] in
  highlight ~terms ~base label
;;

let render_list
  ?(empty_label = "(no notes)")
  ?(terms = [])
  ~width
  ~height
  ~cursor
  (notes : Db.Note.t list)
  =
  let content =
    match notes with
    | [] ->
      View.center (View.text ~attrs:[ Attr.fg dim ] empty_label) ~within:{ width; height }
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
          render_list_row ~terms ~width ~is_selected:(scroll_off + i = cursor) note)
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
let render_detail ?(terms = []) ~width ~height ~scroll (note : Db.Note.t option) =
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
          (highlight
             ~terms
             ~base:[ Attr.fg accent; Attr.bold ]
             (Db.Note.display_title note)
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
      View.vcat (header :: View.text "" :: List.map visible ~f:(highlight ~terms ~base:[]))
  in
  fit ~width ~height content
;;

(* The list pane title: in Browse it's just "Notes"; in Search it echoes the live query
   with a cursor marker (▏) so you can see what you're typing and where the edit cursor
   sits. [budget] is the column space the border box leaves for the title; the query
   window scrolls horizontally to keep the cursor visible, so a long query stays usable
   without the marker ever being split (the query buffer is pure ASCII, the marker is
   multibyte). *)
let list_title ~budget (model : Model.t) =
  match model.mode with
  | Browse | Help -> "Notes"
  | Search ->
    let prefix = "Search: " in
    let { Editor.buf; cursor } = model.editor in
    let cursor = Int.clamp_exn cursor ~min:0 ~max:(String.length buf) in
    (* Columns left for the query text after the label and the 1-col marker. *)
    let avail = Int.max 1 (budget - String.length prefix - 1) in
    (* Slide a window over [buf] so [cursor] is always inside it. *)
    let start = Int.max 0 (cursor - avail) in
    let len = Int.min avail (String.length buf - start) in
    let window = String.sub buf ~pos:start ~len in
    let rel = cursor - start in
    let before = String.prefix window rel in
    let after = String.drop_prefix window rel in
    [%string "%{prefix}%{before}▏%{after}"]
;;

(* The keybinding cheat-sheet, as (keys, description) rows grouped under headings. Mirrors
   docs/decisions.md "Keyboard conventions"; keep the two in sync when bindings change. *)
let help_sections =
  [ ( "Navigation"
    , [ "C-n / C-p", "next / previous note"
      ; "C-a / C-e", "first / last note"
      ; "Tab", "switch list / detail pane"
      ; "Enter", "focus the detail pane"
      ] )
  ; "Detail pane", [ "C-n / C-p", "scroll body down / up"; "C-a / C-e", "top / bottom" ]
  ; ( "Search"
    , [ "/", "start full-text search"
      ; "C-b / C-f", "move cursor left / right"
      ; "C-k", "kill to end of line"
      ; "C-w", "kill word backward"
      ; "C-d / DEL", "delete char forward / back"
      ; "Enter", "commit query, focus results"
      ; "Esc", "cancel search"
      ] )
  ; "General", [ "?", "toggle this help"; "C-g / Esc / q", "close help" ]
  ]
;;

(* Render the help overlay: a bordered box listing every binding, centered over the panes.
   The key column is padded to a fixed width so descriptions line up. Width/height are
   computed from the content so the box hugs the text; [center] places it on the screen. *)
let render_help ~width ~height =
  let key_col =
    List.concat_map help_sections ~f:(fun (_, rows) -> List.map rows ~f:fst)
    |> List.map ~f:String.length
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let row (keys, desc) =
    View.hcat
      [ View.text ~attrs:[ Attr.fg accent ] (String.pad_right keys ~len:key_col)
      ; View.text "  "
      ; View.text desc
      ]
  in
  let blocks =
    List.concat_map help_sections ~f:(fun (heading, rows) ->
      (View.text ~attrs:[ Attr.fg dim; Attr.bold ] heading :: List.map rows ~f:row)
      @ [ View.text "" ])
  in
  (* Drop the trailing blank line so the box doesn't end with dead space. *)
  let blocks =
    match List.rev blocks with
    | _ :: rest -> List.rev rest
    | [] -> []
  in
  let box =
    Bonsai_term_border_box.view
      ~line_type:Round_corners
      ~attrs:[ Attr.fg accent ]
      ~title:"Keybindings"
      ~title_attrs:[ Attr.fg accent; Attr.bold ]
      (View.pad ~l:1 ~r:1 (View.vcat blocks))
  in
  View.center box ~within:{ width; height }
;;

let app ~(db : Db.t) ~(dimensions : Dimensions.t Bonsai.t) (local_ graph)
  : view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  =
  (* Browse corpus: loaded once for the app's lifetime. The live handle stays open so the
     search path can re-query; see [launch_tui]. *)
  let notes = Db.list_all db in
  let model, set_model =
    Bonsai.state'
      Model.initial
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      graph
  in
  (* Live FTS results, derived from the query buffer. The raw buffer is sanitized to a
     safe MATCH (so half-typed input can't raise an FTS5 error), then [cutoff] on the
     string keeps us from re-querying when the buffer changes in a way that doesn't change
     the query text. An empty sanitized query yields no results (rather than a [""]
     MATCH). *)
  let safe_query =
    Bonsai.cutoff
      ~equal:String.equal
      (let%arr model in
       Fts_query.sanitize model.editor.buf)
  in
  let results =
    let%arr safe_query in
    if String.is_empty safe_query
    then []
    else Db.search db ~query:safe_query ~limit:200 ()
  in
  (* The active list backs both selection and rendering: results in Search, corpus in
     Browse. Cursor/scroll clamping is computed against whichever is active. *)
  let active =
    let%arr model and results in
    match model.mode with
    | Search -> results
    | Browse | Help -> notes
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
  (* A single inject takes the raw key and does mode/focus routing inside [set_model], so
     it reads the live model rather than a handler value that's only as fresh as the last
     view recompute. [count]/[detail_max] for nav clamping come from the *active* list and
     live pane geometry; a one-frame-stale [active] only affects nav clamping (which
     self-corrects next frame), never query editing. *)
  let inject =
    let%arr set_model
    and active
    and _, detail_w, pane_h = panes in
    fun (event : Event.t) ->
      set_model (fun model ->
        match route model event with
        | None -> model
        | Some action ->
          let count = List.length active in
          let selected = List.nth active model.cursor in
          let detail_max = detail_max_scroll ~width:detail_w ~height:pane_h selected in
          apply_action_pure ~count ~detail_max model action)
  in
  let view =
    let%arr model
    and active
    and { Dimensions.width; height } = dimensions
    and list_w, detail_w, pane_h = panes in
    let selected = List.nth active model.cursor in
    let list_focused = [%equal: Focus.t] model.focus List in
    let searching = [%equal: Mode.t] model.mode Search in
    (* Terms to emphasize: the query words, lowercased, only while searching. Empty in
       browse, so [highlight] is a no-op there. *)
    let terms =
      if searching
      then Fts_query.tokens model.editor.buf |> List.map ~f:String.lowercase
      else []
    in
    (* The border box lays [title] into the top edge as-is (no truncation), so callers
       must size it to the pane — [list_title ~budget] does that for the search query. The
       [╭ ] prefix and trailing [ ─╮] consume ~4 cols; the [<tab>] hint (shown on the
       unfocused pane) eats more, so its width is folded into the budget below. *)
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
    let tab_cols = if list_focused then 0 else String.length " <tab>" in
    let list_box =
      box
        ~focused:list_focused
        ~title:(list_title ~budget:(list_w - 4 - tab_cols) model)
        (render_list
           ~empty_label:(if searching then "(no matches)" else "(no notes)")
           ~terms
           ~width:list_w
           ~height:pane_h
           ~cursor:model.cursor
           active)
    in
    let detail_box =
      box
        ~focused:(not list_focused)
        ~title:"Detail"
        (render_detail
           ~terms
           ~width:detail_w
           ~height:pane_h
           ~scroll:model.detail_scroll
           selected)
    in
    let content = View.hcat [ list_box; detail_box ] in
    (* In Help mode the cheat-sheet sits on top of the panes, which stay visible behind
       it. [zcat] draws earlier elements on top, so the overlay goes first. *)
    let overlay =
      match model.mode with
      | Help -> [ render_help ~width ~height ]
      | Browse | Search -> []
    in
    (* Backdrop so the framed panes sit on a full-screen rectangle. *)
    View.zcat (overlay @ [ content; View.rectangle ~width ~height () ])
  in
  (* The handler just forwards every key to [inject]; all mode/focus routing happens in
     the reducer against the live model (see [route] / [inject]). *)
  let handler =
    let%arr inject in
    fun (event : Event.t) -> inject event
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

(* The DB handle stays open for the whole TUI run so the search path can re-query live.
   [with_db]'s synchronous bracket won't do here: [app] reads the corpus and then the
   Bonsai loop keeps querying until the user quits, so we must close only after the run's
   deferred resolves, not when [Bonsai_term.start] returns its (still-pending) deferred. *)
let launch_tui db_path =
  let db = Db.open_ db_path in
  Async.Monitor.protect
    ~finally:(fun () ->
      Db.close db;
      Async.return ())
    (fun () -> Bonsai_term.start (app ~db))
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
