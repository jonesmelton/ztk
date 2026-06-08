open! Core
open! Bonsai_term

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

(* Print a note list the way the headless subcommands report results: one line per note,
   tab-separated, so output is greppable and stable. *)
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

(* The DB handle stays open for the whole TUI run so the search path can re-query live.
   [with_db]'s synchronous bracket won't do here: [app] reads the corpus and then the
   Bonsai loop keeps querying until the user quits, so we must close only after the run's
   deferred resolves, not when [Bonsai_term.start] returns its (still-pending) deferred. *)
let launch_tui db_path =
  let db = Db.open_ db_path in
  Async.Monitor.protect
    ~finally:(fun () ->
      Db.close db;
      Async.return ())
    (fun () -> Bonsai_term.start (App.app ~db))
;;

(* Synchronous entry point for the no-subcommand group body, which runs outside the Async
   scheduler and must return [unit]. Boots the scheduler, runs the TUI, and raises on
   error. *)
let launch_tui_blocking db_path =
  match Async.Thread_safe.block_on_async (fun () -> launch_tui db_path) with
  | Ok (Ok ()) -> ()
  | Ok (Error e) -> Error.raise e
  | Error exn -> raise exn
;;

(* The interactive browser. Mirrors what bare `zet` does, but lets you pass -db. Bare
   `zet` (no subcommand) runs this with the default path; see [command]. *)
let tui_command =
  Async.Command.async_or_error
    ~summary:"launch the interactive TUI browser"
    (let%map_open.Command db_path = db_path_flag in
     fun () -> launch_tui db_path)
;;

(* Headless mirror of the list pane: [Db.list_all] or [Db.list_recent]. *)
let list_command =
  Command.basic
    ~summary:"list notes (headless mirror of the TUI list pane)"
    ~readme:(fun () ->
      "Prints notes as: id<TAB>slug<TAB>kind<TAB>title. With -recent N, shows the\n\
       N most recent by entry date; otherwise all notes ordered by id.")
    (let%map_open.Command db_path = db_path_flag
     and recent =
       flag "-recent" (optional int) ~doc:"N show the N most recent notes instead of all"
     in
     fun () ->
       let notes =
         Db.with_db db_path ~f:(fun db ->
           match recent with
           | Some limit -> Db.list_recent db ~limit
           | None -> Db.list_all db)
       in
       print_notes notes)
;;

(* Resolve an IDENT (as accepted by [show]/[edit]) to a note: a numeric string is tried as
   an id first, then as a slug; a non-numeric string is a slug. Shared so every
   IDENT-taking subcommand resolves the same way. *)
let resolve_note db ident =
  match Int.of_string_opt ident with
  | Some id ->
    (match Db.get_by_id db id with
     | Some _ as n -> n
     | None -> Db.get_by_slug db ident)
  | None -> Db.get_by_slug db ident
;;

(* Headless mirror of opening a note in the detail pane: print its full body. IDENT is a
   numeric id or a slug; numeric strings are tried as id first. *)
let show_command =
  Command.basic
    ~summary:"print a single note's body (headless mirror of the TUI detail pane)"
    ~readme:(fun () ->
      "IDENT is a note id (integer) or a slug. Prints a metadata header followed\n\
       by the note body. Exits nonzero if no note matches.")
    (let%map_open.Command db_path = db_path_flag
     and ident = anon ("IDENT" %: string) in
     fun () ->
       let note = Db.with_db db_path ~f:(fun db -> resolve_note db ident) in
       match note with
       | None ->
         prerr_endline (sprintf "no note matching %S" ident);
         exit 1
       | Some n ->
         printf "# %s\n" (Db.Note.display_title n);
         printf "id: %d\tkind: %s" n.id n.kind;
         Option.iter n.slug ~f:(printf "\tslug: %s");
         Option.iter n.entry_date ~f:(printf "\tdate: %s");
         Out_channel.newline stdout;
         Out_channel.newline stdout;
         print_string n.body;
         if not (String.is_suffix n.body ~suffix:"\n") then Out_channel.newline stdout)
;;

(* Headless mirror of [Db.search] — full-text search printed to stdout. Every TUI
   capability gets a subcommand like this so the CLI is a complete surface, scriptable
   without the terminal UI. *)
let search_command =
  Command.basic
    ~summary:"full-text search notes (headless mirror of the TUI search)"
    ~readme:(fun () ->
      "QUERY is a raw FTS5 MATCH expression (e.g. 'ocaml', 'type NEAR system').\n\
       Prints matching notes, best-ranked first, as: id<TAB>slug<TAB>kind<TAB>title.")
    (let%map_open.Command db_path = db_path_flag
     and kind = flag "-kind" (optional string) ~doc:"KIND restrict to journal|note|inbox"
     and limit =
       flag "-limit" (optional_with_default 50 int) ~doc:"N max results (default: 50)"
     and query = anon ("QUERY" %: string) in
     fun () ->
       let notes =
         Db.with_db db_path ~f:(fun db -> Db.search db ~query ?kind ~limit ())
       in
       print_notes notes)
;;

(* Headless mirror of editing a note's body in the TUI. IDENT resolves like [show]. The
   new body comes from [-body] (inline), else [-file PATH], else stdin — so it scripts as
   [echo ... | zet edit 5] or [zet edit my-slug -file note.md]. Naive overwrite, no
   revision history (see [Db.update_body]). Exits nonzero if no note matches. *)
let edit_command =
  Command.basic
    ~summary:"replace a note's body (headless mirror of the TUI editor)"
    ~readme:(fun () ->
      "IDENT is a note id (integer) or a slug. The replacement body is read from\n\
       -body STR if given, else from -file PATH, else from stdin. Overwrites the\n\
       body in place (no revision history). Exits nonzero if no note matches.")
    (let%map_open.Command db_path = db_path_flag
     and ident = anon ("IDENT" %: string)
     and body = flag "-body" (optional string) ~doc:"STR new body text (inline)"
     and file =
       flag "-file" (optional string) ~doc:"PATH read the new body from this file"
     in
     fun () ->
       let new_body =
         match body, file with
         | Some b, _ -> b
         | None, Some path -> In_channel.read_all path
         | None, None -> In_channel.input_all In_channel.stdin
       in
       Db.with_db db_path ~f:(fun db ->
         match resolve_note db ident with
         | None ->
           prerr_endline (sprintf "no note matching %S" ident);
           exit 1
         | Some n -> Db.update_body db ~id:n.id ~body:new_body))
;;

(* Top-level: bare `zet` launches the TUI (default db); subcommands are the headless
   mirrors. New features add a peer subcommand here. *)
let command =
  Command.group
    ~summary:{|zet — zettelkasten TUI + headless CLI|}
    ~readme:(fun () ->
      "A read-mostly terminal UI over a personal zettelkasten (SQLite), with a\n\
       headless subcommand for every TUI capability. Run `zet` with no\n\
       subcommand to launch the browser on the default database.")
    ~body:(fun ~path:_ -> launch_tui_blocking default_db_path)
    [ "tui", tui_command
    ; "list", list_command
    ; "show", show_command
    ; "search", search_command
    ; "edit", edit_command
    ]
;;
