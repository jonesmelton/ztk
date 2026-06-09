open! Core
open! Bonsai_term

module Focus = struct
  type t =
    | List
    | Detail
  [@@deriving sexp_of, equal]
end

module Mode = struct
  type t =
    | Browse
    | Search
    | Help (* full-screen keybinding cheat-sheet overlay; dismiss back to Browse *)
    | Edit (* editing the selected note's body; the text editor captures the keyboard *)
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
    ; editing_id : int option
        (* id of the note being edited in [Edit] mode; pins the save target so a corpus
           reload can't change which note a save writes to. The [C-x C-s] chord's
           armed-bit lives in a separate [Bonsai.state_machine] ([chord] in [app]), not
           here, so it stays batch-safe.
        *)
    ; mark : int option
        (* Emacs-style mark for the body editor: a codepoint offset into the editor buffer
           set by [C-Space]. [None] = no mark. The region is
           [min mark cursor, max mark cursor]. Only meaningful in [Edit] mode; cleared on
           copy and on edit exit. *)
    ; kill_ring : string option
    (* One-slot kill ring: the text of the last copied region. Stands in for a system
       clipboard (no platform dep, testable); [M-w] writes it. *)
    }
  [@@deriving sexp_of, equal]

  let initial =
    { cursor = 0
    ; focus = List
    ; detail_scroll = 0
    ; mode = Browse
    ; editor = Editor.empty
    ; editing_id = None
    ; mark = None
    ; kill_ring = None
    }
  ;;
end

module Action = struct
  type t =
    | Up
    | Down
    | Top
    | Bottom
    | Toggle_focus
    | Focus_detail
    | Start_search
    | Exit_search
    | Open_help
    | Close_help
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

let route (model : Model.t) (event : Event.t) : Action.t option =
  match model.mode with
  | Help ->
    (match event with
     | Key_press { key = Escape; mods = _ }
     | Key_press { key = ASCII 'G'; mods = [ Ctrl ] }
     | Key_press { key = ASCII '?'; mods = _ }
     | Key_press { key = ASCII 'q'; mods = [] } -> Some Close_help
     | _ -> None)
  (* Emacs editing keybindings from strace_ui in
     https://github.com/janestreet/bonsai_term_examples *)
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
  | Edit -> None
;;

(* Word-boundary logic from strace_ui in
   https://github.com/janestreet/bonsai_term_examples *)
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
