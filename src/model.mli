open! Core
open! Bonsai_term

module Focus : sig
  type t =
    | List
    | Detail
  [@@deriving sexp_of, equal]
end

module Mode : sig
  type t =
    | Browse
    | Search
    | Help
    | Edit
    | Extract (* naming the note extracted from a marked region; editor holds the title *)
  [@@deriving sexp_of, equal]
end

(** An in-flight region extraction: the marked slice has been removed from the source
    note's body in memory but nothing is persisted yet. [new_body] is the slice (becomes
    the new note's body); [source_body] is the source note's buffer with the slice
    removed; [source_id] pins the save target. The title is entered live in the shared
    editor. Committed atomically via [Db.extract_region]. *)
module Extract : sig
  type t =
    { source_id : int
    ; source_body : string
    ; new_body : string
    }
  [@@deriving sexp_of, equal]
end

(** The live query line: [buf] is the raw text, [cursor] a byte offset into it (0..len). *)
module Editor : sig
  type t =
    { buf : string
    ; cursor : int
    }
  [@@deriving sexp_of, equal]

  val empty : t
end

module Model : sig
  type t =
    { cursor : int
    ; focus : Focus.t
    ; detail_scroll : int
    ; mode : Mode.t
    ; editor : Editor.t
    ; editing_id : int option
    ; mark : int option
    ; kill_ring : string option
    ; extract : Extract.t option
    }
  [@@deriving sexp_of, equal]

  val initial : t
end

module Action : sig
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

(** Derive a slug from a title: lowercased, accented Latin-1 letters folded to ASCII, runs
    of everything outside [a-z0-9] collapsed to single dashes, ends trimmed. [None] when
    nothing survives (blank or punctuation-only), so the caller stores a NULL slug. *)
val slug_of_title : string -> string option

(** [split_region ~buf ~lo ~hi] returns [(slice, remainder)] where [slice] is the
    codepoint range [\[lo, hi)] of [buf] and [remainder] is [buf] with that slice removed.
    Offsets are codepoints (matching editor cursor positions), so multibyte text slices
    cleanly. [lo]/[hi] are sorted, so order doesn't matter. *)
val split_region : buf:string -> lo:int -> hi:int -> string * string

(** [extract_lines ~body ~lo ~hi] lifts the 1-based inclusive line range [[lo, hi]] out of
    [body], returning [(slice, remainder)]. The slice is the selected lines with outer
    blank lines trimmed (interior blanks and indentation preserved); the remainder is the
    surviving lines rejoined so the gap closes. [lo]/[hi] are clamped to the line count
    and sorted. Line-granular for the headless CLI, unlike the codepoint-granular
    [split_region]. *)
val extract_lines : body:string -> lo:int -> hi:int -> string * string

(** Map a raw key to an action given the current mode. [None] for keys with no binding in
    the current mode. Pure: reads only the model and the event. *)
val route : Model.t -> Event.t -> Action.t option

(** Pure reducer. [count] is the active-list size and [detail_max] the largest valid
    detail scroll offset for the selected note; both are threaded in so motion can clamp
    without the model carrying the notes or their wrapped geometry. *)
val apply_action_pure : count:int -> detail_max:int -> Model.t -> Action.t -> Model.t
