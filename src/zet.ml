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
    | notes ->
      List.map notes ~f:(fun n -> Printf.sprintf "%d  %s" n.id (Db.Note.display_title n))
  in
  let view =
    let%arr dimensions in
    let body = View.vcat (List.map lines ~f:View.text) in
    View.center ~within:dimensions body
  in
  let handler = Bonsai.return (fun _ -> Effect.Ignore) in
  ~view, ~handler
;;

let default_db_path =
  match Sys.getenv "ZET_DB" with
  | Some path -> path
  | None ->
    (match Sys.getenv "HOME" with
     | Some home -> home ^/ ".zet/zet.db"
     | None -> "zet.db")
;;

let db_path_flag =
  let%map_open.Command db_path =
    flag
      "-db"
      (optional_with_default default_db_path string)
      ~doc:
        (sprintf
           "PATH path to the zettelkasten SQLite file (default: %s; or $ZET_DB)"
           default_db_path)
  in
  db_path
;;

(* Print a note list the way the headless subcommands report results: one line
   per note, tab-separated, so output is greppable and stable. *)
let print_notes (notes : Db.Note.t list) =
  List.iter notes ~f:(fun n ->
    print_endline
      (String.concat
         ~sep:"\t"
         [ Int.to_string n.id
         ; Option.value n.slug ~default:""
         ; n.kind
         ; Db.Note.display_title n
         ]))
;;

let launch_tui db_path =
  let notes = Db.with_db db_path ~f:Db.list_all in
  Bonsai_term.start (app ~notes)
;;

(* Synchronous entry point for the no-subcommand group body, which runs outside
   the Async scheduler and must return [unit]. Boots the scheduler, runs the
   TUI, and raises on error. *)
let launch_tui_blocking db_path =
  match Async.Thread_safe.block_on_async (fun () -> launch_tui db_path) with
  | Ok (Ok ()) -> ()
  | Ok (Error e) -> Error.raise e
  | Error exn -> raise exn
;;

(* The interactive browser. Mirrors what bare `zet` does, but lets you pass -db.
   Bare `zet` (no subcommand) runs this with the default path; see [command]. *)
let tui_command =
  Async.Command.async_or_error
    ~summary:"launch the interactive TUI browser"
    (let%map_open.Command db_path = db_path_flag in
     fun () -> launch_tui db_path)
;;

(* Headless mirror of [Db.search] — full-text search printed to stdout.
   Every TUI capability gets a subcommand like this so the CLI is a complete
   surface, scriptable without the terminal UI. *)
let search_command =
  Command.basic
    ~summary:"full-text search notes (headless mirror of the TUI search)"
    ~readme:(fun () ->
      "QUERY is a raw FTS5 MATCH expression (e.g. 'ocaml', 'type NEAR system').\n\
       Prints matching notes, best-ranked first, as: id<TAB>slug<TAB>kind<TAB>title.")
    (let%map_open.Command db_path = db_path_flag
     and kind =
       flag "-kind" (optional string) ~doc:"KIND restrict to journal|note|inbox"
     and limit =
       flag "-limit" (optional_with_default 50 int) ~doc:"N max results (default: 50)"
     and query = anon ("QUERY" %: string) in
     fun () ->
       let notes = Db.with_db db_path ~f:(fun db -> Db.search db ~query ?kind ~limit ()) in
       print_notes notes)
;;

(* Top-level: bare `zet` launches the TUI (default db); subcommands are the
   headless mirrors. New features add a peer subcommand here. *)
let command =
  Command.group
    ~summary:{|zet — zettelkasten TUI + headless CLI|}
    ~readme:(fun () ->
      "A read-mostly terminal UI over a personal zettelkasten (SQLite), with a\n\
       headless subcommand for every TUI capability. Run `zet` with no\n\
       subcommand to launch the browser on the default database.")
    ~body:(fun ~path:_ -> launch_tui_blocking default_db_path)
    [ "tui", tui_command; "search", search_command ]
;;
