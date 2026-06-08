open! Core
open! Bonsai_term

(** The interactive browser. Takes an open DB handle (kept alive for the app's lifetime so
    the search path can re-query live) rather than a snapshot corpus. *)
val app
  :  db:Db.t
  -> dimensions:Dimensions.t Bonsai.t
  -> Bonsai.graph @ local
  -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
