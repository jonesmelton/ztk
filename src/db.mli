open! Core

module Note : sig
  type t =
    { id : int
    ; slug : string
    ; kind : string
    ; title : string
    ; body : string
    ; entry_date : string option
    ; metadata : string option
    }
  [@@deriving sexp_of, fields]
end

type t

(** Open the SQLite database at [path]. Single-writer; plain connection. *)
val open_ : string -> t

val close : t -> unit

(** Run [f] with a connection to [path], closing it afterward. *)
val with_db : string -> f:(t -> 'a) -> 'a

(** Execute a multi-statement SQL script (schema/seed/migration) for its side
    effects. Raises on the first failing statement. *)
val exec_script : t -> string -> unit

(** All notes, ordered by id. Hard-coded query for the initial wiring. *)
val list_all : t -> Note.t list
