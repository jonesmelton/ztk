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

(* [with_db] closes synchronously; the Bonsai loop keeps querying until the user quits, so
   we close only after the deferred from [Bonsai_term.start] resolves. *)
let launch_tui db_path =
  let db = Db.open_ db_path in
  Async.Monitor.protect
    ~finally:(fun () ->
      Db.close db;
      Async.return ())
    (fun () -> Bonsai_term.start (App.app ~db))
;;

let launch_tui_blocking db_path =
  match Async.Thread_safe.block_on_async (fun () -> launch_tui db_path) with
  | Ok (Ok ()) -> ()
  | Ok (Error e) -> Error.raise e
  | Error exn -> raise exn
;;

let tui_command =
  Async.Command.async_or_error
    ~summary:"launch the interactive TUI browser"
    (let%map_open.Command db_path = db_path_flag in
     fun () -> launch_tui db_path)
;;

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

let resolve_note db ident =
  match Int.of_string_opt ident with
  | Some id ->
    (match Db.get_by_id db id with
     | Some _ as n -> n
     | None -> Db.get_by_slug db ident)
  | None -> Db.get_by_slug db ident
;;

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

let parse_line_range s =
  match String.lsplit2 s ~on:'-' with
  | Some (lo, hi) ->
    (match Int.of_string_opt (String.strip lo), Int.of_string_opt (String.strip hi) with
     | Some lo, Some hi -> Some (lo, hi)
     | _ -> None)
  | None ->
    (* A bare "N" is the single-line range N-N. *)
    Option.map (Int.of_string_opt (String.strip s)) ~f:(fun n -> n, n)
;;

let extract_command =
  Command.basic
    ~summary:"extract a line range into a new note (headless mirror of TUI C-x C-e)"
    ~readme:(fun () ->
      "IDENT is the source note's id (integer) or slug. -lines M-N selects the\n\
       1-based inclusive line range to lift out (a bare N means just line N); use\n\
       `zet show IDENT` to read off line numbers. The selected lines become a new\n\
       note's body (outer blank lines trimmed); the source note keeps the rest with\n\
       the gap closed. -title names the new note (its slug is derived from the\n\
       title; omitted = untitled with no slug). The source trim and the new note are\n\
       written in one transaction — neither lands without the other. Prints the new\n\
       note's id. Exits nonzero if no note matches or the range is empty/invalid.")
    (let%map_open.Command db_path = db_path_flag
     and ident = anon ("IDENT" %: string)
     and lines = flag "-lines" (required string) ~doc:"M-N 1-based inclusive line range"
     and title = flag "-title" (optional string) ~doc:"STR title for the new note" in
     fun () ->
       match parse_line_range lines with
       | None ->
         prerr_endline (sprintf "invalid -lines %S (expected M-N or N)" lines);
         exit 1
       | Some (lo, hi) ->
         Db.with_db db_path ~f:(fun db ->
           match resolve_note db ident with
           | None ->
             prerr_endline (sprintf "no note matching %S" ident);
             exit 1
           | Some source ->
             let new_body, source_body = Model.extract_lines ~body:source.body ~lo ~hi in
             if String.is_empty new_body
             then (
               prerr_endline
                 (sprintf "line range %d-%d is empty in note %d" lo hi source.id);
               exit 1);
             let title =
               Option.bind title ~f:(fun t ->
                 match String.strip t with
                 | "" -> None
                 | t -> Some t)
             in
             let slug = Option.bind title ~f:Model.slug_of_title in
             let new_id =
               Db.extract_region
                 db
                 ~source_id:source.id
                 ~source_body
                 ~new_slug:slug
                 ~new_kind:"note"
                 ~new_title:title
                 ~new_body
                 ~new_entry_date:None
                 ~new_metadata:None
             in
             printf "%d\n" new_id))
;;

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
    ; "extract", extract_command
    ]
;;
