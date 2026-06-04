open! Core
open! Bonsai_term

module Db = Db

val app
  :  notes:Db.Note.t list
  -> dimensions:Dimensions.t Bonsai.t
  -> Bonsai.graph @ local
  -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t

val command : Command.t
