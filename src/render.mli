open! Core
open! Bonsai_term

(** Pane accent color (focused frames, selection, highlight base). *)
val accent : Attr.Color.t

(** Dimmed color for metadata, placeholders, and unfocused frames. *)
val dim : Attr.Color.t

(** Pin [view] to exactly [width] x [height] by backing it with a transparent rectangle,
    so a pane's frame sizes to the pane, not to the longest content line. *)
val fit : width:int -> height:int -> View.t -> View.t

(** Greedy word-wrap to [width] columns; words longer than [width] are hard-split. A blank
    input line stays one blank line. Pure — shared by the renderer and scroll-clamp. *)
val wrap_line : width:int -> string -> string list

(** The detail body as a flat list of display lines: each source line word-wrapped to
    [width], blank lines preserved. *)
val detail_body_lines : width:int -> string -> string list

(** Largest valid detail scroll offset for [note] at the given pane geometry: wrapped body
    line count minus visible body rows, floored at 0. Shared with the reducer so a scroll
    offset can never point past the end of the body. *)
val detail_max_scroll : width:int -> height:int -> Db.Note.t option -> int

(** The list pane: selection marker + note labels, search terms emphasized, scrolled to
    keep [cursor] near the middle, pinned to the pane geometry. *)
val render_list
  :  ?empty_label:string
  -> ?terms:string list
  -> width:int
  -> height:int
  -> cursor:int
  -> Db.Note.t list
  -> View.t

(** The detail pane: fixed header (title/slug/kind/date) + scrollable word-wrapped body,
    sliced by [scroll] (a wrapped-line offset). *)
val render_detail
  :  ?terms:string list
  -> width:int
  -> height:int
  -> scroll:int
  -> Db.Note.t option
  -> View.t

(** The list pane title: "Notes" in Browse, or the live query with a cursor marker in
    Search, windowed to [budget] columns so the marker is never split. *)
val list_title : budget:int -> Model.Model.t -> string

(** The keybinding cheat-sheet overlay, centered within [width] x [height]. *)
val render_help : width:int -> height:int -> View.t
