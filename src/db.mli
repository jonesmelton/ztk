open! Core

module Note : sig
  (** [slug] and [title] are nullable in the real schema — untitled journal entries have
      neither. [kind] and [body] are always present in practice. *)
  type t =
    { id : int
    ; slug : string option
    ; kind : string
    ; title : string option
    ; body : string
    ; entry_date : string option
    ; metadata : string option
    }
  [@@deriving sexp_of, fields]

  (** A human label for lists/headers: [title] if present, else [entry_date], else [#id].
      Always non-empty. *)
  val display_title : t -> string
end

type t

(** Open the SQLite database at [path]. Single-writer; plain connection. *)
val open_ : string -> t

val close : t -> unit

(** Run [f] with a connection to [path], closing it afterward. *)
val with_db : string -> f:(t -> 'a) -> 'a

(** Execute a multi-statement SQL script (schema/seed/migration) for its side effects.
    Raises on the first failing statement. *)
val exec_script : t -> string -> unit

(** Run [f] inside a single SQL transaction: [begin] first, then [commit] if [f] returns
    normally, or [rollback] if it raises (the exception is re-raised). Use this to make a
    multi-statement write atomic — either all of [f]'s statements land or none do. Not
    reentrant: SQLite has no nested transactions, so [f] must not call [with_txn]. *)
val with_txn : t -> f:(t -> 'a) -> 'a

(** All notes, ordered by id. Hard-coded query for the initial wiring. *)
val list_all : t -> Note.t list

(** Most recent notes first (by [entry_date] then [id]), capped at [limit]. *)
val list_recent : t -> limit:int -> Note.t list

(** The note with this id, if any. *)
val get_by_id : t -> int -> Note.t option

(** The note with this slug, if any. *)
val get_by_slug : t -> string -> Note.t option

(** Overwrite the body of the note with [id]. The FTS index is kept in sync by the
    [notes_au] trigger. Naive overwrite with no revision history. *)
val update_body : t -> id:int -> body:string -> unit

(** Insert a new note and return its id. [slug], [title], [entry_date], and [metadata] are
    nullable; [kind] must satisfy the schema's check constraint
    ('journal'|'note'|'inbox'). The FTS index is kept in sync by the [notes_ai] trigger. *)
val create_note
  :  t
  -> slug:string option
  -> kind:string
  -> title:string option
  -> body:string
  -> entry_date:string option
  -> metadata:string option
  -> int

(** Extract a region into a new note, atomically. In one transaction: create a new note
    from the [new_*] fields, then overwrite the source note ([source_id]) body with
    [source_body] (the remainder after the slice was removed). Returns the new note's id.
    If either write fails, the whole operation rolls back — the source is never trimmed
    without the new note also landing. Body-level revision history is out of scope here. *)
val extract_region
  :  t
  -> source_id:int
  -> source_body:string
  -> new_slug:string option
  -> new_kind:string
  -> new_title:string option
  -> new_body:string
  -> new_entry_date:string option
  -> new_metadata:string option
  -> int

(** Full-text search via the [notes_fts] FTS5 index. [query] is a raw FTS5 MATCH
    expression; results are ranked best-first and capped at [limit]. [kind], if given,
    restricts to that note kind. *)
val search : t -> query:string -> ?kind:string -> limit:int -> unit -> Note.t list

(** Tags of [note], read from its metadata JSON [$.tags]. [] if none. *)
val tags_of : t -> Note.t -> string list

(** Notes whose metadata [$.tags] array contains [tag], ordered by id. *)
val filter_by_tag : t -> tag:string -> Note.t list
