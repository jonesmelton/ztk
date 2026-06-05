open! Core
open! Bonsai_term
module Db = Db

val app
  :  notes:Db.Note.t list
  -> dimensions:Dimensions.t Bonsai.t
  -> Bonsai.graph @ local
  -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t

val command : Command.t

(** Print notes to stdout as [id<TAB>slug<TAB>kind<TAB>title], one per line — the stable,
    greppable format the headless subcommands emit. *)
val print_notes : Db.Note.t list -> unit
