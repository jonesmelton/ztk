open! Core
open! Bonsai_term
module Db = Db

(** Turns arbitrary live keystrokes into a guaranteed-valid FTS5 MATCH expression so a
    half-typed query can never raise a syntax error. Each whitespace-separated token is
    stripped of FTS5-special characters and re-emitted as a quoted prefix phrase; an
    all-empty input yields [""]. *)
module Fts_query : sig
  val sanitize : string -> string
end

(** The interactive browser. Takes an open DB handle (kept alive for the app's lifetime so
    the search path can re-query live) rather than a snapshot corpus. *)
val app
  :  db:Db.t
  -> dimensions:Dimensions.t Bonsai.t
  -> Bonsai.graph @ local
  -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t

val command : Command.t

(** Print notes to stdout as [id<TAB>slug<TAB>kind<TAB>title], one per line — the stable,
    greppable format the headless subcommands emit. *)
val print_notes : Db.Note.t list -> unit
