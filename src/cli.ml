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
       N most recent by entry date; otherwise all notes ordered by id. Soft-deleted\n\
       notes are hidden unless -all is given.")
    (let%map_open.Command db_path = db_path_flag
     and recent =
       flag "-recent" (optional int) ~doc:"N show the N most recent notes instead of all"
     and all =
       flag "-all" no_arg ~doc:" include soft-deleted notes (hidden by default)"
     in
     fun () ->
       let notes =
         Db.with_db db_path ~f:(fun db ->
           match recent with
           | Some limit -> Db.list_recent db ~limit
           | None -> Db.list_all ~include_deleted:all db)
       in
       print_notes notes)
;;

let valid_kinds = [ "journal"; "note"; "inbox" ]

(* Comma-split tags into the metadata JSON the web forms write: [{"tags":[...]}].
   Empty/blank entries dropped; no tags at all yields [None] so the column stays NULL
   (matching the Ruby [meta = tags.empty? ? nil : ...]). *)
let metadata_of_tags tags_csv : string option =
  let tags =
    String.split tags_csv ~on:','
    |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  match tags with
  | [] -> None
  | tags ->
    Some
      (Yojson.Safe.to_string
         (`Assoc [ "tags", `List (List.map tags ~f:(fun t -> `String t)) ]))
;;

let today_iso () = Date.to_string (Date.today ~zone:(Lazy.force Time_float.Zone.local))

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

let read_body ~body ~file =
  match body, file with
  | Some b, _ -> b
  | None, Some path -> In_channel.read_all path
  | None, None -> In_channel.input_all In_channel.stdin
;;

let create_command =
  Command.basic
    ~summary:"create a new note (headless mirror of the web new-note form)"
    ~readme:(fun () ->
      "Inserts a note and prints its new id. -kind is required (journal|note|inbox).\n\
       The body is read from -body STR, else -file PATH, else stdin. -tags is a\n\
       comma-separated list stored as metadata {\"tags\":[...]}.\n\n\
       Slug/date policy mirrors the web form: kind=note requires -title and derives a\n\
       unique slug from it (foo, foo-2, ...); kind=journal sets entry_date to today\n\
       (override with -entry-date); kind=inbox gets neither slug nor date. Pass -slug\n\
       to set the slug explicitly (still uniquified). Exits nonzero on a bad kind or a\n\
       note with no usable title.")
    (let%map_open.Command db_path = db_path_flag
     and kind = flag "-kind" (required string) ~doc:"KIND journal|note|inbox"
     and title = flag "-title" (optional string) ~doc:"STR note title"
     and slug =
       flag "-slug" (optional string) ~doc:"STR explicit slug (default: from title)"
     and entry_date =
       flag
         "-entry-date"
         (optional string)
         ~doc:"YYYY-MM-DD journal date (default: today)"
     and tags = flag "-tags" (optional string) ~doc:"CSV comma-separated tags -> metadata"
     and body = flag "-body" (optional string) ~doc:"STR body text (inline)"
     and file = flag "-file" (optional string) ~doc:"PATH read the body from this file" in
     fun () ->
       if not (List.mem valid_kinds kind ~equal:String.equal)
       then (
         prerr_endline (sprintf "invalid -kind %S (expected journal|note|inbox)" kind);
         exit 1);
       let clean s =
         Option.bind s ~f:(fun s ->
           match String.strip s with
           | "" -> None
           | s -> Some s)
       in
       let title = clean title in
       let new_body = read_body ~body ~file in
       let metadata = Option.bind tags ~f:metadata_of_tags in
       Db.with_db db_path ~f:(fun db ->
         let slug, entry_date =
           match kind with
           | "note" ->
             let base =
               match clean slug, Option.bind title ~f:Model.slug_of_title with
               | Some s, _ -> Model.slug_of_title s
               | None, derived -> derived
             in
             (match base with
              | None ->
                prerr_endline
                  "kind=note needs a -title (or -slug) with at least one letter or digit";
                exit 1
              | Some base -> Some (Db.unique_slug db base), None)
           | "journal" ->
             None, Some (Option.value (clean entry_date) ~default:(today_iso ()))
           | _ -> None, clean entry_date
         in
         let id =
           Db.create_note db ~slug ~kind ~title ~body:new_body ~entry_date ~metadata
         in
         printf "%d\n" id))
;;

let edit_command =
  Command.basic
    ~summary:
      "edit a note: body and/or its kind/title/slug/tags (headless mirror of the TUI \
       editor + web edit form)"
    ~readme:(fun () ->
      "IDENT is a note id (integer) or a slug.\n\n\
       With none of -kind/-title/-slug/-tags given, this is a body-only overwrite (the\n\
       TUI-editor mirror): the new body comes from -body STR, else -file PATH, else\n\
       stdin. With any metadata flag given, it mirrors the web edit form and rewrites\n\
       the whole row; the body then defaults to the note's existing body unless -body\n\
       or -file is supplied (so you can change just the title without re-sending the\n\
       body), and -tags replaces the tag set ('' clears it).\n\n\
       Kind/slug/date policy matches the web form: changing -kind to note keeps the old\n\
       slug if it was already a note, else derives a unique one from -title; -kind\n\
       journal keeps the old entry_date or sets today; -kind inbox drops slug and date.\n\
       -slug overrides the derived slug (still uniquified). Overwrites in place (no\n\
       revision history). Exits nonzero if no note matches.")
    (let%map_open.Command db_path = db_path_flag
     and ident = anon ("IDENT" %: string)
     and kind = flag "-kind" (optional string) ~doc:"KIND change kind: journal|note|inbox"
     and title = flag "-title" (optional string) ~doc:"STR new title"
     and slug =
       flag "-slug" (optional string) ~doc:"STR explicit slug (default: from title)"
     and tags =
       flag "-tags" (optional string) ~doc:"CSV replace tags (empty string clears them)"
     and body = flag "-body" (optional string) ~doc:"STR new body text (inline)"
     and file =
       flag "-file" (optional string) ~doc:"PATH read the new body from this file"
     in
     fun () ->
       let metadata_edit =
         Option.is_some kind
         || Option.is_some title
         || Option.is_some slug
         || Option.is_some tags
       in
       Db.with_db db_path ~f:(fun db ->
         match resolve_note db ident with
         | None ->
           prerr_endline (sprintf "no note matching %S" ident);
           exit 1
         | Some n when not metadata_edit ->
           Db.update_body db ~id:n.id ~body:(read_body ~body ~file)
         | Some n ->
           let clean s =
             Option.bind s ~f:(fun s ->
               match String.strip s with
               | "" -> None
               | s -> Some s)
           in
           let new_kind = Option.value (clean kind) ~default:n.kind in
           if not (List.mem valid_kinds new_kind ~equal:String.equal)
           then (
             prerr_endline
               (sprintf "invalid -kind %S (expected journal|note|inbox)" new_kind);
             exit 1);
           (* Body source: explicit -body/-file, else keep the existing body. *)
           let new_body =
             match body, file with
             | Some b, _ -> b
             | None, Some path -> In_channel.read_all path
             | None, None -> n.body
           in
           (* New title: -title if given (cleaned), else the note's current title. *)
           let new_title =
             match title with
             | Some _ -> clean title
             | None -> n.title
           in
           (* tags: given -> replace (possibly clearing to NULL); absent -> keep existing. *)
           let new_metadata =
             match tags with
             | Some csv -> metadata_of_tags csv
             | None -> n.metadata
           in
           let new_slug, new_entry_date =
             match new_kind with
             | "note" ->
               (* Reuse the old slug only if it was already a note; else derive from the
                  explicit -slug or the title. *)
               let base =
                 match clean slug with
                 | Some s -> Model.slug_of_title s
                 | None ->
                   if String.equal n.kind "note" && Option.is_some n.slug
                   then n.slug
                   else Option.bind new_title ~f:Model.slug_of_title
               in
               (match base with
                | None ->
                  prerr_endline
                    "kind=note needs a -title (or -slug) with at least one letter or \
                     digit";
                  exit 1
                | Some base ->
                  (* Don't re-suffix the note's own current slug. *)
                  let slug =
                    if Option.equal String.equal (Some base) n.slug
                    then base
                    else Db.unique_slug db base
                  in
                  Some slug, None)
             | "journal" ->
               let date = if String.equal n.kind "journal" then n.entry_date else None in
               None, Some (Option.value date ~default:(today_iso ()))
             | _ -> None, None
           in
           Db.update_note
             db
             ~id:n.id
             ~slug:new_slug
             ~kind:new_kind
             ~title:new_title
             ~body:new_body
             ~entry_date:new_entry_date
             ~metadata:new_metadata))
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
       title; omitted = untitled with no slug). If -title's slug matches an existing\n\
       note, the slice is appended to that note instead of creating a new one. The\n\
       source trim and the new-note/append write happen in one transaction — neither\n\
       lands without the other. Prints the target note's id (new or appended-to).\n\
       Exits nonzero if no note matches or the range is empty/invalid.")
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
             (* Mirror the TUI: a -title whose slug matches an existing note appends the
                slice to that note (printing its id); otherwise create a new note. slug is
                UNIQUE, so the lookup returns at most one target. *)
             (match Option.bind slug ~f:(fun slug -> Db.get_by_slug db slug) with
              | Some (target : Db.Note.t) ->
                Db.append_region
                  db
                  ~source_id:source.id
                  ~source_body
                  ~target_id:target.id
                  ~slice:new_body;
                printf "%d\n" target.id
              | None ->
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
                printf "%d\n" new_id)))
;;

let restore_command =
  Command.basic
    ~summary:"clear a note's soft-delete mark (un-delete)"
    ~readme:(fun () ->
      "IDENT is a note id (integer) or a slug. Removes the $.deleted marker so the\n\
       note reappears in the default list and TUI corpus. Use `zet list -all` to find\n\
       soft-deleted notes. Exits nonzero if no note matches.")
    (let%map_open.Command db_path = db_path_flag
     and ident = anon ("IDENT" %: string) in
     fun () ->
       Db.with_db db_path ~f:(fun db ->
         match resolve_note db ident with
         | None ->
           prerr_endline (sprintf "no note matching %S" ident);
           exit 1
         | Some n -> Db.set_deleted db ~id:n.id ~deleted:false))
;;

let sweep_command =
  Command.basic
    ~summary:"permanently delete all soft-deleted notes"
    ~readme:(fun () ->
      "Hard-deletes every note marked for deletion (those carrying a $.deleted\n\
       marker, e.g. via the TUI `d` key). This is irreversible — there is no\n\
       revision history. Prints the number of notes removed. Review first with\n\
       `zet list -all`; un-delete individual notes with `zet restore IDENT`.")
    (let%map_open.Command db_path = db_path_flag in
     fun () ->
       let n = Db.with_db db_path ~f:Db.sweep_deleted in
       printf "swept %d note%s\n" n (if n = 1 then "" else "s"))
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
    ; "create", create_command
    ; "edit", edit_command
    ; "extract", extract_command
    ; "restore", restore_command
    ; "sweep", sweep_command
    ]
;;
