open! Core

(** The full [zet] command: bare invocation launches the TUI on the default database;
    subcommands ([tui], [list], [show], [search], [edit], [extract], [restore], [sweep])
    are headless mirrors. *)
val command : Command.t

(** Print notes to stdout as [id<TAB>slug<TAB>kind<TAB>title], one per line — the stable,
    greppable format the headless subcommands emit. *)
val print_notes : Db.Note.t list -> unit

(** Machine-readable [list]/[search] output: one JSON object per line, each carrying the
    raw row fields ([id], [kind], [slug], [title], [entry_date], [metadata], [tags]) plus
    a body [snippet]. Nullable columns serialize as JSON [null]; [metadata] is the
    column's JSON parsed back to a value ([null] if absent or unparseable). *)
val print_notes_json : Db.Note.t list -> unit

(** Machine-readable [show] output: a single JSON object with the same raw fields as
    {!print_notes_json} but the full [body] instead of a snippet. *)
val print_note_json : Db.Note.t -> unit

(** Resolve an IDENT to a note the way the [show]/[edit] subcommands do: a numeric string
    is tried as an id first, then as a slug; a non-numeric string is a slug. [None] if
    nothing matches. *)
val resolve_note : Db.t -> string -> Db.Note.t option

(** Parse the [extract -lines] argument: ["M-N"] -> [Some (m, n)], a bare ["N"] ->
    [Some (n, n)], anything else -> [None]. Does not validate the range against any note. *)
val parse_line_range : string -> (int * int) option
