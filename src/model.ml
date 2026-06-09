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
    | Extract
      (* naming the note extracted from a marked region; the editor holds the new title,
         the slice + source remainder are pinned in [Model.extract] *)
  [@@deriving sexp_of, equal]
end

(* The in-flight extraction: a marked region has been sliced out of the source note's body
   but nothing is persisted yet. [new_body] is the slice (the new note's body),
   [source_body] is what the source note keeps (its buffer with the slice removed), and
   [source_id] pins the save target. The new title is entered live in the shared editor,
   so it is not stored here. Committed atomically by [Db.extract_region]. *)
module Extract = struct
  type t =
    { source_id : int
    ; source_body : string
    ; new_body : string
    }
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
    ; extract : Extract.t option
    (* [Some] only in [Extract] mode: the pending region-extraction awaiting a title and
       save. [None] otherwise. *)
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
    ; extract = None
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
  | Edit | Extract -> None
;;

(* Kebab-case a title into a slug: lowercase, ASCII-fold common Latin-1 letters, keep
   [a-z0-9], collapse every other run into a single dash, trim leading/trailing dashes.
   [None] when nothing survives (empty/blank/punctuation-only) so the caller stores NULL. *)
let slug_of_title title : string option =
  (* Fold a Unicode codepoint to an ASCII slug char, or [None] to treat it as a separator.
     Iterates codepoints (not bytes): an accented letter like 'ü' is one codepoint,
     whereas its UTF-8 encoding is two bytes — folding per-byte would mangle it into
     separators. *)
  let fold_code code : char option =
    if code <= 0x7f
    then (
      match Char.lowercase (Char.of_int_exn code) with
      | ('a' .. 'z' | '0' .. '9') as c -> Some c
      | _ -> None)
    else (
      match code with
      | 0xe0 | 0xe1 | 0xe2 | 0xe3 | 0xe4 | 0xe5 | 0xc0 | 0xc1 | 0xc2 | 0xc3 | 0xc4 | 0xc5
        -> Some 'a'
      | 0xe8 | 0xe9 | 0xea | 0xeb | 0xc8 | 0xc9 | 0xca | 0xcb -> Some 'e'
      | 0xec | 0xed | 0xee | 0xef | 0xcc | 0xcd | 0xce | 0xcf -> Some 'i'
      | 0xf2 | 0xf3 | 0xf4 | 0xf5 | 0xf6 | 0xd2 | 0xd3 | 0xd4 | 0xd5 | 0xd6 -> Some 'o'
      | 0xf9 | 0xfa | 0xfb | 0xfc | 0xd9 | 0xda | 0xdb | 0xdc -> Some 'u'
      | 0xf1 | 0xd1 -> Some 'n'
      | _ -> None)
  in
  let buf = Buffer.create (String.length title) in
  Zed.Zed_utf8.iter
    (fun zc ->
      match fold_code (Zed.Zed_char.code zc) with
      | Some c -> Buffer.add_char buf c
      | None ->
        (* Emit at most one separator dash per run; never lead with one. *)
        (match Buffer.length buf with
         | 0 -> ()
         | n ->
           if not (Char.equal (Buffer.nth buf (n - 1)) '-') then Buffer.add_char buf '-'))
    title;
  let s = Buffer.contents buf |> String.rstrip ~drop:(Char.equal '-') in
  if String.is_empty s then None else Some s
;;

(* Split [buf] at the codepoint range [lo, hi): the slice is [buf[lo, hi)], the remainder
   is the buffer with that slice removed. Codepoint offsets (not bytes) so multibyte text
   slices cleanly — matches the editor's cursor [position] semantics. *)
let split_region ~buf ~lo ~hi : string * string =
  let lo, hi = Int.min lo hi, Int.max lo hi in
  let slice = Zed.Zed_utf8.sub buf lo (hi - lo) in
  let before = Zed.Zed_utf8.sub buf 0 lo in
  let after = Zed.Zed_utf8.sub buf hi (Zed.Zed_utf8.length buf - hi) in
  slice, before ^ after
;;

(* Extract a 1-based inclusive line range [lo, hi] from [body], returning
   [(slice, remainder)]. The slice is the selected lines joined by newlines with leading
   and trailing *blank* lines trimmed (interior blanks and per-line indentation kept). The
   remainder is the lines outside the range rejoined, so the gap closes cleanly. [lo]/[hi]
   are clamped to the body's line count; an empty/blank selection yields an empty slice.
   Line-granular (used by the headless CLI), unlike the codepoint-granular [split_region]
   the TUI uses. *)
let extract_lines ~body ~lo ~hi : string * string =
  let lines = String.split_lines body in
  let n = List.length lines in
  let lo = Int.clamp_exn lo ~min:1 ~max:(Int.max 1 n) in
  let hi = Int.clamp_exn hi ~min:1 ~max:(Int.max 1 n) in
  let lo, hi = Int.min lo hi, Int.max lo hi in
  let picked, kept =
    List.partitioni_tf lines ~f:(fun i _ ->
      let one_based = i + 1 in
      one_based >= lo && one_based <= hi)
  in
  let is_blank s = String.is_empty (String.strip s) in
  let slice =
    picked
    |> List.drop_while ~f:is_blank
    |> List.rev
    |> List.drop_while ~f:is_blank
    |> List.rev
    |> String.concat ~sep:"\n"
  in
  slice, String.concat kept ~sep:"\n"
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
