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

(** Notes ordered by id. Soft-deleted notes (those carrying a [$.deleted] timestamp in
    their metadata) are excluded by default; pass [~include_deleted:true] to return them
    too (used by [list --all] and the sweep path). [kind], if given, restricts to that
    note kind. *)
val list_all : ?include_deleted:bool -> ?kind:string -> t -> Note.t list

(** Most recent notes first (by [entry_date] then [id]), capped at [limit]. [kind], if
    given, restricts to that note kind before the limit is applied. *)
val list_recent : ?kind:string -> t -> limit:int -> Note.t list

(** The note with this id, if any. *)
val get_by_id : t -> int -> Note.t option

(** The note with this slug, if any. *)
val get_by_slug : t -> string -> Note.t option

(** Whether any note already uses [slug]. slug is UNIQUE, so this answers whether an
    insert or update carrying [slug] would collide. *)
val slug_exists : t -> string -> bool

(** A free slug derived from [base]: [base] itself if unused, else the first [base-N] (N
    starting at 2) that is unused. Mirrors the web forms' collision suffixing. *)
val unique_slug : t -> string -> string

(** Overwrite the body of the note with [id]. The FTS index is kept in sync by the
    [notes_au] trigger. Naive overwrite with no revision history. *)
val update_body : t -> id:int -> body:string -> unit

(** Full-field overwrite of the note with [id], mirroring the web [note-edit] POST:
    rewrites [slug], [kind], [title], [body], [entry_date], [metadata] and bumps
    [updated_at]. Any slug/entry_date derivation policy is the caller's; this writes
    exactly what it is given. [kind] must satisfy the schema check
    ('journal'|'note'|'inbox'). The [notes_au] trigger keeps FTS in sync. No revision
    history. *)
val update_note
  :  t
  -> id:int
  -> slug:string option
  -> kind:string
  -> title:string option
  -> body:string
  -> entry_date:string option
  -> metadata:string option
  -> unit

(** Soft-delete or restore the note with [id] by setting or clearing a [$.deleted]
    timestamp in its metadata JSON. Marking ([~deleted:true]) hides it from the default
    [list_all] corpus but leaves the row intact and recoverable; unmarking
    ([~deleted:false]) restores it. The actual row is only removed by [sweep_deleted]. The
    [notes_au] trigger keeps the FTS index in sync. *)
val set_deleted : t -> id:int -> deleted:bool -> unit

(** Hard-delete every soft-deleted note (those with a [$.deleted] metadata timestamp) in a
    single transaction, returning the number of rows removed. Irreversible — this is the
    only path that deletes rows. The [notes_ad] trigger drops the corresponding FTS rows. *)
val sweep_deleted : t -> int

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

(** Append a region to an existing note, atomically. In one transaction: append [slice] to
    [target_id]'s body (joined by a blank line), then overwrite the source note
    ([source_id]) body with [source_body] (the remainder after the slice was removed).
    Raises if [target_id] does not exist, rolling back so the source is never trimmed
    without the append landing. The [notes_au] trigger keeps FTS in sync on both writes. *)
val append_region
  :  t
  -> source_id:int
  -> source_body:string
  -> target_id:int
  -> slice:string
  -> unit

(** Full-text search via the [notes_fts] FTS5 index. [query] is a raw FTS5 MATCH
    expression; results are ranked best-first and capped at [limit]. [kind], if given,
    restricts to that note kind. *)
val search : t -> query:string -> ?kind:string -> limit:int -> unit -> Note.t list

(** Tags of [note], read from its metadata JSON [$.tags]. [] if none. *)
val tags_of : t -> Note.t -> string list

(** Notes whose metadata [$.tags] array contains [tag], ordered by id. *)
val filter_by_tag : t -> tag:string -> Note.t list
