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

(** Map a raw key to an action given the current mode. [None] for keys with no binding in
    the current mode. Pure: reads only the model and the event. *)
val route : Model.t -> Event.t -> Action.t option

(** Pure reducer. [count] is the active-list size and [detail_max] the largest valid
    detail scroll offset for the selected note; both are threaded in so motion can clamp
    without the model carrying the notes or their wrapped geometry. *)
val apply_action_pure : count:int -> detail_max:int -> Model.t -> Action.t -> Model.t
