open! Core

(** The full [zet] command: bare invocation launches the TUI on the default database;
    subcommands ([tui], [list], [show], [search], [edit]) are headless mirrors. *)
val command : Command.t

(** Print notes to stdout as [id<TAB>slug<TAB>kind<TAB>title], one per line — the stable,
    greppable format the headless subcommands emit. *)
val print_notes : Db.Note.t list -> unit

(** Resolve an IDENT to a note the way the [show]/[edit] subcommands do: a numeric string
    is tried as an id first, then as a slug; a non-numeric string is a slug. [None] if
    nothing matches. *)
val resolve_note : Db.t -> string -> Db.Note.t option
