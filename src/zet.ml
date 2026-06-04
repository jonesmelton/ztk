open! Core
open! Bonsai_term
open Bonsai.Let_syntax
module Db = Db

let app ~(notes : Db.Note.t list) ~(dimensions : Dimensions.t Bonsai.t) (local_ _graph)
  : view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  =
  let lines =
    match notes with
    | [] -> [ "(no notes)" ]
    | notes -> List.map notes ~f:(fun n -> Printf.sprintf "%d  %s" n.id n.title)
  in
  let view =
    let%arr dimensions in
    let body = View.vcat (List.map lines ~f:View.text) in
    View.center ~within:dimensions body
  in
  let handler = Bonsai.return (fun _ -> Effect.Ignore) in
  ~view, ~handler
;;

let command =
  Async.Command.async_or_error
    ~summary:{|zet — zettelkasten TUI|}
    (let%map_open.Command db_path =
       flag "-db" (required string) ~doc:"PATH path to the zettelkasten SQLite file"
     in
     fun () ->
       let notes = Db.with_db db_path ~f:Db.list_all in
       Bonsai_term.start (app ~notes))
;;
