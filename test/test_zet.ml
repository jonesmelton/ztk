open! Core
open Bonsai_test
open Bonsai_term
module Db = Zet.Db

let key ?(mods = []) k = Event.Key_press { key = k; mods }

let seeded_db () =
  let db = Db.open_ ":memory:" in
  Db.exec_script db (In_channel.read_all "../db/schema.sql");
  Db.exec_script db (In_channel.read_all "../db/seed.sql");
  db
;;

let show_titles notes =
  List.iter notes ~f:(fun (n : Db.Note.t) ->
    let slug = Option.value n.slug ~default:"-" in
    print_endline
      (Printf.sprintf "%d %-14s %-7s %s" n.id slug n.kind (Db.Note.display_title n)))
;;

let%expect_test "list_all returns every seeded note, ordered by id" =
  let db = seeded_db () in
  let notes = Db.list_all db in
  Db.close db;
  show_titles notes;
  [%expect
    {|
    1 hello-zet      note    Hello, zet
    2 ocaml-notes    note    OCaml type system
    3 morning-pages  journal Morning pages
    4 untagged-inbox inbox   Quick capture
    5 -              journal 2026-05-28
    |}]
;;

let%expect_test "list_recent orders by entry_date desc and honors limit" =
  let db = seeded_db () in
  let notes = Db.list_recent db ~limit:2 in
  Db.close db;
  show_titles notes;
  [%expect
    {|
    1 hello-zet      note    Hello, zet
    3 morning-pages  journal Morning pages
    |}]
;;

let%expect_test "get_by_id and get_by_slug" =
  let db = seeded_db () in
  let by_id = Db.get_by_id db 2 in
  let by_slug = Db.get_by_slug db "morning-pages" in
  let missing = Db.get_by_id db 999 in
  Db.close db;
  print_s
    [%sexp
      ((by_id, by_slug, missing) : Db.Note.t option * Db.Note.t option * Db.Note.t option)];
  [%expect
    {|
    (((
       (id 2)
       (slug (ocaml-notes))
       (kind note)
       (title ("OCaml type system"))
       (body "Notes on the OCaml module system and functors.")
       (entry_date (2026-06-01))
       (metadata   ("{\"tags\":[\"ocaml\",\"demo\"]}"))))
     ((
       (id 3)
       (slug (morning-pages))
       (kind journal)
       (title ("Morning pages"))
       (body "A journal entry that also mentions OCaml in passing.")
       (entry_date (2026-06-02))
       (metadata   ("{\"tags\":[\"journal\"]}"))))
     ())
    |}]
;;

let%expect_test "update_body persists and reindexes FTS" =
  let db = seeded_db () in
  Db.update_body db ~id:2 ~body:"Rewritten body mentioning kakapo.";
  let reread = Db.get_by_id db 2 in
  print_s [%sexp (Option.map reread ~f:Db.Note.body : string option)];
  [%expect {| ("Rewritten body mentioning kakapo.") |}];
  print_endline "-- search new text --";
  show_titles (Db.search db ~query:"kakapo" ~limit:10 ());
  print_endline "-- search stale text --";
  show_titles (Db.search db ~query:"functors" ~limit:10 ());
  Db.close db;
  [%expect
    {|
    -- search new text --
    2 ocaml-notes    note    OCaml type system
    -- search stale text --
    |}]
;;

let%expect_test "with_txn commits all statements on success" =
  let db = seeded_db () in
  Db.with_txn db ~f:(fun db ->
    Db.update_body db ~id:1 ~body:"txn body one";
    Db.update_body db ~id:2 ~body:"txn body two");
  let body id = Option.bind (Db.get_by_id db id) ~f:(fun n -> Some n.body) in
  print_s [%sexp ((body 1, body 2) : string option * string option)];
  Db.close db;
  [%expect {|
    (("txn body one")
     ("txn body two"))
    |}]
;;

let%expect_test "with_txn rolls back every statement when f raises" =
  let db = seeded_db () in
  let body id = Option.bind (Db.get_by_id db id) ~f:(fun n -> Some n.body) in
  let original_1 = body 1 in
  (* First write succeeds, then we raise: the commit must never happen, so note 1's body
     must revert to its pre-transaction value. *)
  let raised =
    Exn.does_raise (fun () ->
      Db.with_txn db ~f:(fun db ->
        Db.update_body db ~id:1 ~body:"should be rolled back";
        failwith "boom"))
  in
  print_s [%sexp (raised : bool)];
  print_s [%sexp ([%equal: string option] (body 1) original_1 : bool)];
  Db.close db;
  [%expect {|
    true
    true
    |}]
;;

let%expect_test "create_note inserts and returns a searchable note" =
  let db = seeded_db () in
  let id =
    Db.create_note
      db
      ~slug:(Some "extracted-kakapo")
      ~kind:"note"
      ~title:(Some "Kakapo facts")
      ~body:"The kakapo is a flightless parrot."
      ~entry_date:None
      ~metadata:None
  in
  let created = Db.get_by_id db id in
  print_s [%sexp (created : Db.Note.t option)];
  print_endline "-- fts sees the new body --";
  show_titles (Db.search db ~query:"kakapo" ~limit:10 ());
  Db.close db;
  [%expect
    {|
    ((
      (id 6)
      (slug (extracted-kakapo))
      (kind note)
      (title ("Kakapo facts"))
      (body "The kakapo is a flightless parrot.")
      (entry_date ())
      (metadata   ())))
    -- fts sees the new body --
    6 extracted-kakapo note    Kakapo facts
    |}]
;;

let%expect_test "create_note allows null slug and title" =
  let db = seeded_db () in
  let id =
    Db.create_note
      db
      ~slug:None
      ~kind:"journal"
      ~title:None
      ~body:"Untitled capture."
      ~entry_date:(Some "2026-06-09")
      ~metadata:None
  in
  print_s [%sexp (Db.get_by_id db id : Db.Note.t option)];
  Db.close db;
  [%expect
    {|
    ((
      (id 6)
      (slug ())
      (kind journal)
      (title ())
      (body "Untitled capture.")
      (entry_date (2026-06-09))
      (metadata ())))
    |}]
;;

let%expect_test "unique_slug returns the base when free, else the first open suffix" =
  let db = seeded_db () in
  (* 'hello-zet' and 'ocaml-notes' are seeded; 'fresh' is not. *)
  print_endline (Db.unique_slug db "fresh");
  print_endline (Db.unique_slug db "hello-zet");
  (* Once hello-zet-2 also exists, the next request skips to -3. *)
  let (_ : int) =
    Db.create_note
      db
      ~slug:(Some "hello-zet-2")
      ~kind:"note"
      ~title:None
      ~body:"x"
      ~entry_date:None
      ~metadata:None
  in
  print_endline (Db.unique_slug db "hello-zet");
  Db.close db;
  [%expect {|
    fresh
    hello-zet-2
    hello-zet-3
    |}]
;;

let%expect_test "update_note rewrites every form-owned field and reindexes FTS" =
  let db = seeded_db () in
  Db.update_note
    db
    ~id:4
    ~slug:(Some "now-a-note")
    ~kind:"note"
    ~title:(Some "Reclassified inbox")
    ~body:"Now mentions kakapo."
    ~entry_date:None
    ~metadata:(Some {|{"tags":["reclassified"]}|});
  print_s [%sexp (Db.get_by_id db 4 : Db.Note.t option)];
  print_endline "-- fts sees the new body --";
  show_titles (Db.search db ~query:"kakapo" ~limit:10 ());
  print_endline "-- tags read back --";
  print_s
    [%sexp
      (Option.value_map (Db.get_by_id db 4) ~default:[] ~f:(Db.tags_of db) : string list)];
  Db.close db;
  [%expect
    {|
    ((
      (id 4)
      (slug (now-a-note))
      (kind note)
      (title ("Reclassified inbox"))
      (body "Now mentions kakapo.")
      (entry_date ())
      (metadata ("{\"tags\":[\"reclassified\"]}"))))
    -- fts sees the new body --
    4 now-a-note     note    Reclassified inbox
    -- tags read back --
    (reclassified)
    |}]
;;

let%expect_test "extract_region atomically creates the new note and trims the source" =
  let db = seeded_db () in
  (* Source note 1 body becomes the remainder; the extracted slice becomes a new note.
     Both writes happen in one transaction. *)
  let new_id =
    Db.extract_region
      db
      ~source_id:1
      ~source_body:"Hello, zet remainder."
      ~new_slug:(Some "from-hello")
      ~new_kind:"note"
      ~new_title:(Some "Extracted bit")
      ~new_body:"the extracted slice"
      ~new_entry_date:None
      ~new_metadata:None
  in
  print_endline "-- source trimmed --";
  print_s [%sexp (Option.map (Db.get_by_id db 1) ~f:Db.Note.body : string option)];
  print_endline "-- new note --";
  print_s [%sexp (Db.get_by_id db new_id : Db.Note.t option)];
  Db.close db;
  [%expect
    {|
    -- source trimmed --
    ("Hello, zet remainder.")
    -- new note --
    ((
      (id 6)
      (slug (from-hello))
      (kind note)
      (title ("Extracted bit"))
      (body "the extracted slice")
      (entry_date ())
      (metadata   ())))
    |}]
;;

let%expect_test "extract_region rolls back the source trim if the insert fails" =
  let db = seeded_db () in
  let original = Option.map (Db.get_by_id db 1) ~f:Db.Note.body in
  (* A duplicate slug violates the UNIQUE constraint on the INSERT, which must abort the
     whole transaction — leaving the source note's body untouched. *)
  let raised =
    Exn.does_raise (fun () ->
      Db.extract_region
        db
        ~source_id:1
        ~source_body:"trimmed body that must not persist"
        ~new_slug:(Some "ocaml-notes") (* already taken by note 2 *)
        ~new_kind:"note"
        ~new_title:None
        ~new_body:"orphan"
        ~new_entry_date:None
        ~new_metadata:None)
  in
  print_s [%sexp (raised : bool)];
  print_endline "-- source unchanged --";
  print_s
    [%sexp
      ([%equal: string option] (Option.map (Db.get_by_id db 1) ~f:Db.Note.body) original
       : bool)];
  print_endline "-- no orphan inserted --";
  print_s [%sexp (List.length (Db.list_all db) : int)];
  Db.close db;
  [%expect
    {|
    true
    -- source unchanged --
    true
    -- no orphan inserted --
    5
    |}]
;;

let%expect_test "append_region appends the slice to the target and trims the source \
                 atomically"
  =
  let db = seeded_db () in
  (* Extract a slice out of note 1 (source) and append it to note 2 (target). Both the
     target append and the source trim land in one transaction. The slice joins the
     target's existing body with a blank-line separator. *)
  Db.append_region
    db
    ~source_id:1
    ~source_body:"Hello, zet remainder."
    ~target_id:2
    ~slice:"the appended slice";
  print_endline "-- source trimmed --";
  print_s [%sexp (Option.map (Db.get_by_id db 1) ~f:Db.Note.body : string option)];
  print_endline "-- target appended --";
  print_s [%sexp (Option.map (Db.get_by_id db 2) ~f:Db.Note.body : string option)];
  Db.close db;
  [%expect
    {|
    -- source trimmed --
    ("Hello, zet remainder.")
    -- target appended --
    ("Notes on the OCaml module system and functors.\n\nthe appended slice")
    |}]
;;

let%expect_test "append_region rolls back the source trim if the target write fails" =
  let db = seeded_db () in
  let original = Option.map (Db.get_by_id db 1) ~f:Db.Note.body in
  (* A non-existent target id makes the append a no-op update; we treat that as a failure
     so the source is never trimmed without the append landing. *)
  let raised =
    Exn.does_raise (fun () ->
      Db.append_region
        db
        ~source_id:1
        ~source_body:"trimmed body that must not persist"
        ~target_id:9999
        ~slice:"orphan slice")
  in
  print_s [%sexp (raised : bool)];
  print_endline "-- source unchanged --";
  print_s
    [%sexp
      ([%equal: string option] (Option.map (Db.get_by_id db 1) ~f:Db.Note.body) original
       : bool)];
  Db.close db;
  [%expect {|
    true
    -- source unchanged --
    true
    |}]
;;

let%expect_test "set_deleted hides a note from list_all, restore brings it back" =
  let db = seeded_db () in
  Db.set_deleted db ~id:3 ~deleted:true;
  print_endline "-- after mark: note 3 gone from default corpus --";
  show_titles (Db.list_all db);
  print_endline "-- include_deleted shows it again --";
  show_titles (Db.list_all ~include_deleted:true db);
  Db.set_deleted db ~id:3 ~deleted:false;
  print_endline "-- after restore: note 3 back in default corpus --";
  show_titles (Db.list_all db);
  Db.close db;
  [%expect
    {|
    -- after mark: note 3 gone from default corpus --
    1 hello-zet      note    Hello, zet
    2 ocaml-notes    note    OCaml type system
    4 untagged-inbox inbox   Quick capture
    5 -              journal 2026-05-28
    -- include_deleted shows it again --
    1 hello-zet      note    Hello, zet
    2 ocaml-notes    note    OCaml type system
    3 morning-pages  journal Morning pages
    4 untagged-inbox inbox   Quick capture
    5 -              journal 2026-05-28
    -- after restore: note 3 back in default corpus --
    1 hello-zet      note    Hello, zet
    2 ocaml-notes    note    OCaml type system
    3 morning-pages  journal Morning pages
    4 untagged-inbox inbox   Quick capture
    5 -              journal 2026-05-28
    |}]
;;

let%expect_test "set_deleted marks a note with NULL metadata without clobbering" =
  let db = seeded_db () in
  (* Note 5 is the untitled journal with NULL metadata — json_set(NULL,...) would no-op
     without the coalesce, so confirm the marker actually lands and hides it. *)
  Db.set_deleted db ~id:5 ~deleted:true;
  print_endline "-- note 5 hidden --";
  show_titles (Db.list_all db);
  print_endline "-- metadata now carries $.deleted --";
  print_s
    [%sexp
      (Option.bind (Db.get_by_id db 5) ~f:Db.Note.metadata
       |> Option.map ~f:(String.is_substring ~substring:"deleted")
       : bool option)];
  Db.close db;
  [%expect
    {|
    -- note 5 hidden --
    1 hello-zet      note    Hello, zet
    2 ocaml-notes    note    OCaml type system
    3 morning-pages  journal Morning pages
    4 untagged-inbox inbox   Quick capture
    -- metadata now carries $.deleted --
    (true)
    |}]
;;

let%expect_test "sweep_deleted hard-deletes marked notes and reindexes FTS" =
  let db = seeded_db () in
  Db.set_deleted db ~id:2 ~deleted:true;
  Db.set_deleted db ~id:3 ~deleted:true;
  print_endline "-- sweep removes both, returns count --";
  print_s [%sexp (Db.sweep_deleted db : int)];
  print_endline "-- survivors --";
  show_titles (Db.list_all ~include_deleted:true db);
  print_endline "-- swept note 2's body no longer FTS-searchable --";
  show_titles (Db.search db ~query:"functors" ~limit:10 ());
  print_endline "-- second sweep is a no-op --";
  print_s [%sexp (Db.sweep_deleted db : int)];
  Db.close db;
  [%expect
    {|
    -- sweep removes both, returns count --
    2
    -- survivors --
    1 hello-zet      note    Hello, zet
    4 untagged-inbox inbox   Quick capture
    5 -              journal 2026-05-28
    -- swept note 2's body no longer FTS-searchable --
    -- second sweep is a no-op --
    0
    |}]
;;

let%expect_test "resolve_note resolves by id then slug" =
  let db = seeded_db () in
  let title ident =
    match Zet.Cli.resolve_note db ident with
    | None -> "<none>"
    | Some n -> Db.Note.display_title n
  in
  printf "by id 3:      %s\n" (title "3");
  printf "by slug:      %s\n" (title "ocaml-notes");
  printf "missing id:   %s\n" (title "9999");
  printf "missing slug: %s\n" (title "nope");
  Db.close db;
  [%expect
    {|
    by id 3:      Morning pages
    by slug:      OCaml type system
    missing id:   <none>
    missing slug: <none>
    |}]
;;

let%expect_test "edit composition: resolve by slug then overwrite body" =
  let db = seeded_db () in
  (match Zet.Cli.resolve_note db "ocaml-notes" with
   | None -> print_endline "<unresolved>"
   | Some n -> Db.update_body db ~id:n.id ~body:"replaced via edit path");
  print_s [%sexp (Option.map (Db.get_by_id db 2) ~f:Db.Note.body : string option)];
  Db.close db;
  [%expect {| ("replaced via edit path") |}]
;;

let%expect_test "parse_line_range accepts M-N and bare N" =
  let show s =
    print_s [%sexp ((s, Zet.Cli.parse_line_range s) : string * (int * int) option)]
  in
  show "2-5";
  show "7";
  show " 3 - 9 ";
  show "abc";
  show "1-";
  [%expect
    {|
    (2-5 ((2 5)))
    (7 ((7 7)))
    (" 3 - 9 " ((3 9)))
    (abc ())
    (1- ())
    |}]
;;

let%expect_test "extract composition: resolve, lift a line range, persist atomically" =
  let db = seeded_db () in
  (* Give note 1 a multi-line body, then extract lines 2-3 into a new titled note. *)
  Db.update_body db ~id:1 ~body:"keep one\npick two\npick three\nkeep four";
  let lo, hi = Option.value_exn (Zet.Cli.parse_line_range "2-3") in
  let source = Option.value_exn (Zet.Cli.resolve_note db "hello-zet") in
  let new_body, source_body = Zet.Model.extract_lines ~body:source.body ~lo ~hi in
  let title = Some "Picked Lines" in
  let new_id =
    Db.extract_region
      db
      ~source_id:source.id
      ~source_body
      ~new_slug:(Option.bind title ~f:Zet.Model.slug_of_title)
      ~new_kind:"note"
      ~new_title:title
      ~new_body
      ~new_entry_date:None
      ~new_metadata:None
  in
  print_endline "-- source keeps the surviving lines, gap closed --";
  print_s [%sexp (Option.map (Db.get_by_id db 1) ~f:Db.Note.body : string option)];
  print_endline "-- new note holds the lifted lines, slug from title --";
  print_s [%sexp (Db.get_by_id db new_id : Db.Note.t option)];
  Db.close db;
  [%expect
    {|
    -- source keeps the surviving lines, gap closed --
    ("keep one\nkeep four")
    -- new note holds the lifted lines, slug from title --
    ((
      (id 6)
      (slug (picked-lines))
      (kind note)
      (title ("Picked Lines"))
      (body "pick two\npick three")
      (entry_date ())
      (metadata   ())))
    |}]
;;

let%expect_test "extract composition: a title matching an existing slug appends, no new \
                 note"
  =
  let db = seeded_db () in
  Db.update_body db ~id:1 ~body:"keep one\npick two\npick three\nkeep four";
  let lo, hi = Option.value_exn (Zet.Cli.parse_line_range "2-3") in
  let source = Option.value_exn (Zet.Cli.resolve_note db "hello-zet") in
  let new_body, source_body = Zet.Model.extract_lines ~body:source.body ~lo ~hi in
  (* "OCaml notes" slugs to "ocaml-notes" = note 2; the CLI forks to append. *)
  let slug = Zet.Model.slug_of_title "OCaml notes" in
  (match Option.bind slug ~f:(fun slug -> Db.get_by_slug db slug) with
   | Some (target : Db.Note.t) ->
     Db.append_region
       db
       ~source_id:source.id
       ~source_body
       ~target_id:target.id
       ~slice:new_body
   | None -> failwith "expected an existing slug match");
  print_endline "-- source keeps the surviving lines, gap closed --";
  print_s [%sexp (Option.map (Db.get_by_id db 1) ~f:Db.Note.body : string option)];
  print_endline "-- note 2 received the appended slice --";
  print_s [%sexp (Option.map (Db.get_by_id db 2) ~f:Db.Note.body : string option)];
  print_endline "-- corpus unchanged at 5 (no new note) --";
  print_s [%sexp (List.length (Db.list_all db) : int)];
  Db.close db;
  [%expect
    {|
    -- source keeps the surviving lines, gap closed --
    ("keep one\nkeep four")
    -- note 2 received the appended slice --
    ("Notes on the OCaml module system and functors.\n\npick two\npick three")
    -- corpus unchanged at 5 (no new note) --
    5
    |}]
;;

let%expect_test "search ranks matches and filters by kind" =
  let db = seeded_db () in
  let all = Db.search db ~query:"ocaml" ~limit:10 () in
  let notes_only = Db.search db ~query:"ocaml" ~kind:"note" ~limit:10 () in
  print_endline "-- all kinds --";
  show_titles all;
  print_endline "-- kind=note --";
  show_titles notes_only;
  Db.close db;
  [%expect
    {|
    -- all kinds --
    2 ocaml-notes    note    OCaml type system
    3 morning-pages  journal Morning pages
    -- kind=note --
    2 ocaml-notes    note    OCaml type system
    |}]
;;

let%expect_test "tags_of reads metadata json, empty when absent" =
  let db = seeded_db () in
  let tags_for slug =
    match Db.get_by_slug db slug with
    | None -> [ "<missing>" ]
    | Some n -> Db.tags_of db n
  in
  let demo = tags_for "ocaml-notes" in
  let untagged = tags_for "untagged-inbox" in
  Db.close db;
  print_s [%sexp ((demo, untagged) : string list * string list)];
  [%expect {| ((ocaml demo) ()) |}]
;;

let%expect_test "filter_by_tag returns notes carrying the tag" =
  let db = seeded_db () in
  let demo = Db.filter_by_tag db ~tag:"demo" in
  Db.close db;
  show_titles demo;
  [%expect
    {|
    1 hello-zet      note    Hello, zet
    2 ocaml-notes    note    OCaml type system
    |}]
;;

let%expect_test "print_notes emits the headless tab-separated format" =
  let db = seeded_db () in
  let notes = Db.search db ~query:"ocaml" ~limit:10 () in
  Db.close db;
  Zet.Cli.print_notes notes;
  [%expect
    {|
    2	ocaml-notes	note	OCaml type system
    3	morning-pages	journal	Morning pages
    |}]
;;

let notes_handle ?initial_dimensions () =
  let db = seeded_db () in
  Bonsai_term_test.create_handle ?initial_dimensions (Zet.App.app ~db)
;;

(* Like [notes_handle] but exposes the backing db so a test can assert on persisted state
   after driving the UI. *)
let notes_handle_with_db ?initial_dimensions () =
  let db = seeded_db () in
  db, Bonsai_term_test.create_handle ?initial_dimensions (Zet.App.app ~db)
;;

let%expect_test "app renders two panes with list focused, first note selected" =
  let handle = notes_handle () in
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes ──────────────────────╮╭ Detail <tab> ─────────────────────────────────╮│
    ││> Hello, zet                 ││Hello, zet                                     ││
    ││  OCaml type system          ││#1  note                                       ││
    ││  Morning pages              ││slug: hello-zet                                ││
    ││  Quick capture              ││date: 2026-06-03                               ││
    ││  2026-05-28                 ││                                               ││
    ││                             ││This is the first seeded note. It exists so the││
    ││                             ││TUI has something to render while the data     ││
    ││                             ││layer is wired up.                             ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "C-n moves cursor, Tab switches focus to detail" =
  let handle = notes_handle () in
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'N'));
  Bonsai_term_test.send_event handle (key Tab);
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes <tab> ────────────────╮╭ Detail ───────────────────────────────────────╮│
    ││  Hello, zet                 ││OCaml type system                              ││
    ││> OCaml type system          ││#2  note                                       ││
    ││  Morning pages              ││slug: ocaml-notes                              ││
    ││  Quick capture              ││date: 2026-06-01                               ││
    ││  2026-05-28                 ││                                               ││
    ││                             ││Notes on the OCaml module system and functors. ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "detail pane scrolls its body when focused" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key Tab);
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌──────────────────────────────────────────────────────┐
    │╭ Notes <tab> ──────╮╭ Detail ───────────────────────╮│
    ││> Hello, zet       ││Hello, zet                     ││
    ││  OCaml type system││#1  note                       ││
    ││  Morning pages    ││slug: hello-zet                ││
    ││  Quick capture    ││date: 2026-06-03               ││
    ││  2026-05-28       ││                               ││
    ││                   ││This is the first seeded note. ││
    ││                   ││It exists so the TUI has       ││
    ││                   ││something to render while the  ││
    │╰───────────────────╯╰───────────────────────────────╯│
    └──────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'N'));
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'N'));
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌──────────────────────────────────────────────────────┐
    │╭ Notes <tab> ──────╮╭ Detail ───────────────────────╮│
    ││> Hello, zet       ││Hello, zet                     ││
    ││  OCaml type system││#1  note                       ││
    ││  Morning pages    ││slug: hello-zet                ││
    ││  Quick capture    ││date: 2026-06-03               ││
    ││  2026-05-28       ││                               ││
    ││                   ││It exists so the TUI has       ││
    ││                   ││something to render while the  ││
    ││                   ││data layer is wired up.        ││
    │╰───────────────────╯╰───────────────────────────────╯│
    └──────────────────────────────────────────────────────┘
    |}]
;;

let type_string handle s =
  String.iter s ~f:(fun c -> Bonsai_term_test.send_event handle (key (ASCII c)))
;;

(* The text editor reads-then-writes its buffer, so multiple inserts in one stabilization
   collapse onto the same state. Recomputing between keystrokes mimics a live terminal. *)
let type_string_editor handle s =
  String.iter s ~f:(fun c ->
    Bonsai_term_test.send_event handle (key (ASCII c));
    Handle.recompute_view handle)
;;

let%expect_test "Fts_query.sanitize quotes tokens and strips specials" =
  let show s =
    print_endline (Printf.sprintf "%-12S -> %S" s (Zet.Fts_query.sanitize s))
  in
  show "ocaml";
  show "oc ca";
  show "type-sys";
  show "  spaced   out  ";
  show "";
  show "   ";
  show "\"";
  show "-";
  show "a\"b(c)*:^d";
  [%expect
    {|
    "ocaml"      -> "\"ocaml\"*"
    "oc ca"      -> "\"oc\"* \"ca\"*"
    "type-sys"   -> "\"typesys\"*"
    "  spaced   out  " -> "\"spaced\"* \"out\"*"
    ""           -> ""
    "   "        -> ""
    "\""         -> ""
    "-"          -> ""
    "a\"b(c)*:^d" -> "\"abcd\"*"
    |}]
;;

let%expect_test "slash enters search mode with an empty query" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌──────────────────────────────────────────────────────┐
    │╭ Search: ▏ ────────╮╭ Detail <tab> ─────────────────╮│
    ││                   ││(no note selected)             ││
    ││                   ││                               ││
    ││                   ││                               ││
    ││   (no matches)    ││                               ││
    ││                   ││                               ││
    ││                   ││                               ││
    ││                   ││                               ││
    ││                   ││                               ││
    │╰───────────────────╯╰───────────────────────────────╯│
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "typing a matching query shows ranked results" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "ocaml";
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌──────────────────────────────────────────────────────┐
    │╭ Search: ocaml▏ ───╮╭ Detail <tab> ─────────────────╮│
    ││> OCaml type system││OCaml type system              ││
    ││  Morning pages    ││#2  note                       ││
    ││                   ││slug: ocaml-notes              ││
    ││                   ││date: 2026-06-01               ││
    ││                   ││                               ││
    ││                   ││Notes on the OCaml module      ││
    ││                   ││system and functors.           ││
    ││                   ││                               ││
    │╰───────────────────╯╰───────────────────────────────╯│
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "non-matching query shows (no matches)" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  Handle.recompute_view handle;
  type_string handle "xyzzy";
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    (cursor ())
    ┌──────────────────────────────────────────────────────┐
    │╭ Search: xyzzy▏ ───╮╭ Detail <tab> ─────────────────╮│
    ││                   ││(no note selected)             ││
    ││                   ││                               ││
    ││                   ││                               ││
    ││   (no matches)    ││                               ││
    ││                   ││                               ││
    ││                   ││                               ││
    ││                   ││                               ││
    ││                   ││                               ││
    │╰───────────────────╯╰───────────────────────────────╯│
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "backspace shortens the query and updates results" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "ocamlx";
  Bonsai_term_test.send_event handle (key Backspace);
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌──────────────────────────────────────────────────────┐
    │╭ Search: ocaml▏ ───╮╭ Detail <tab> ─────────────────╮│
    ││> OCaml type system││OCaml type system              ││
    ││  Morning pages    ││#2  note                       ││
    ││                   ││slug: ocaml-notes              ││
    ││                   ││date: 2026-06-01               ││
    ││                   ││                               ││
    ││                   ││Notes on the OCaml module      ││
    ││                   ││system and functors.           ││
    ││                   ││                               ││
    │╰───────────────────╯╰───────────────────────────────╯│
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "escape exits search back to the browse corpus" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "ocaml";
  Bonsai_term_test.send_event handle (key Escape);
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌──────────────────────────────────────────────────────┐
    │╭ Notes ────────────╮╭ Detail <tab> ─────────────────╮│
    ││> Hello, zet       ││Hello, zet                     ││
    ││  OCaml type system││#1  note                       ││
    ││  Morning pages    ││slug: hello-zet                ││
    ││  Quick capture    ││date: 2026-06-03               ││
    ││  2026-05-28       ││                               ││
    ││                   ││This is the first seeded note. ││
    ││                   ││It exists so the TUI has       ││
    ││                   ││something to render while the  ││
    │╰───────────────────╯╰───────────────────────────────╯│
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "select a result and view it in the detail pane" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 12 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "ocaml";
  Bonsai_term_test.send_event handle (key Enter);
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'N'));
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌──────────────────────────────────────────────────────┐
    │╭ Search: l▏ <tab> ─╮╭ Detail ───────────────────────╮│
    ││> OCaml type system││OCaml type system              ││
    ││  Morning pages    ││#2  note                       ││
    ││                   ││slug: ocaml-notes              ││
    ││                   ││date: 2026-06-01               ││
    ││                   ││                               ││
    ││                   ││Notes on the OCaml module      ││
    ││                   ││system and functors.           ││
    ││                   ││                               ││
    ││                   ││                               ││
    ││                   ││                               ││
    │╰───────────────────╯╰───────────────────────────────╯│
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "garbage query does not crash the search" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "\"-*";
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌──────────────────────────────────────────────────────┐
    │╭ Search: "-*▏ ─────╮╭ Detail <tab> ─────────────────╮│
    ││                   ││(no note selected)             ││
    ││                   ││                               ││
    ││                   ││                               ││
    ││   (no matches)    ││                               ││
    ││                   ││                               ││
    ││                   ││                               ││
    ││                   ││                               ││
    ││                   ││                               ││
    │╰───────────────────╯╰───────────────────────────────╯│
    └──────────────────────────────────────────────────────┘
    |}]
;;

let ansi_handle ?initial_dimensions () =
  let db = seeded_db () in
  Bonsai_term_test.create_handle
    ?initial_dimensions
    ~capability:Bonsai_term_test.Capability.Ansi
    (Zet.App.app ~db)
;;

let%expect_test "search highlights matched terms in list and detail" =
  let handle = ansi_handle ~initial_dimensions:{ width = 54; height = 9 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "oc";
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    (off fg:cyan)╭(off fg:cyan +bold) Search: oc▏ (off fg:cyan)──────(off fg:cyan)╮(off fg:gray)╭(off fg:gray +bold) Detail <tab> (off fg:gray)─────────────────(off fg:gray)╮
    (off fg:cyan)│(off fg:cyan +bold)> (off fg:yellow +bold)OCaml(off fg:cyan +bold) (off fg:cyan +bold)type(off fg:cyan +bold) (off fg:cyan +bold)system(off fg:cyan)│(off fg:gray)│(off fg:yellow +bold)OCaml(off fg:cyan +bold) (off fg:cyan +bold)type(off fg:cyan +bold) (off fg:cyan +bold)system(off fg:gray)              (off fg:gray)│
    (off fg:cyan)│(off)  (off)Morning(off) (off)pages(off fg:cyan)    (off fg:cyan)│(off fg:gray)│(off fg:gray)#2  note(off fg:gray)                       (off fg:gray)│
    (off fg:cyan)│(off fg:cyan)                   (off fg:cyan)│(off fg:gray)│(off fg:gray)slug: ocaml-notes(off fg:gray)              (off fg:gray)│
    (off fg:cyan)│(off fg:cyan)                   (off fg:cyan)│(off fg:gray)│(off fg:gray)date: 2026-06-01(off fg:gray)               (off fg:gray)│
    (off fg:cyan)│(off fg:cyan)                   (off fg:cyan)│(off fg:gray)│(off fg:gray)                               (off fg:gray)│
    (off fg:cyan)│(off fg:cyan)                   (off fg:cyan)│(off fg:gray)│(off)Notes(off) (off)on(off) (off)the(off) (off fg:yellow +bold)OCaml(off) (off)module(off fg:gray)      (off fg:gray)│
    (off fg:cyan)│(off fg:cyan)                   (off fg:cyan)│(off fg:gray)│(off)system(off) (off)and(off) (off)functors(off).(off fg:gray)           (off fg:gray)│
    (off fg:cyan)╰(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)─(off fg:cyan)╯(off fg:gray)╰(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)─(off fg:gray)╯(off)
    |}]
;;

let%expect_test "? opens and closes the help overlay" =
  let handle = notes_handle () in
  Bonsai_term_test.send_event handle (key (ASCII '?'));
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes ──────────────────────╮╭ Detail <tab> ─────────────────────────────────╮│
    ││> Hello, zet                 ││Hello, zet                                     ││
    ││  OCaml type system          ││#1  note                                       ││
    ││  Morning╭ Keybindings ─────────────────────────────────────────────╮         ││
    ││  Quick c│ Navigation                                               │         ││
    ││  2026-05│ C-n / C-p      next / previous note                      │         ││
    ││         │ C-a / C-e      first / last note                         │ts so the││
    ││         │ Tab            switch list / detail pane                 │data     ││
    ││         │ Enter          focus the detail pane                     │         ││
    ││         │                                                          │         ││
    ││         │ Detail pane                                              │         ││
    ││         │ C-n / C-p      scroll body down / up                     │         ││
    ││         │ C-a / C-e      top / bottom                              │         ││
    ││         │                                                          │         ││
    ││         │ Search                                                   │         ││
    ││         │ /              start full-text search                    │         ││
    ││         │ C-b / C-f      move cursor left / right                  │         ││
    ││         │ C-k            kill to end of line                       │         ││
    ││         │ C-w            kill word backward                        │         ││
    ││         │ C-d / DEL      delete char forward / back                │         ││
    ││         │ Enter          commit query, focus results               │         ││
    ││         │ Esc            cancel search                             │         ││
    ││         │                                                          │         ││
    ││         │ Editing                                                  │         ││
    ││         │ e              edit selected note's body                 │         ││
    ││         │ d              soft-delete note (sweep/restore headless) │         ││
    ││         │ C-Space        set mark (start region)                   │         ││
    ││         │ M-w            copy region                               │         ││
    ││         │ C-y            yank (paste) copied text                  │         ││
    ││         │ C-x C-e        extract region to a new note              │         ││
    ││         │ C-x C-s        save changes                              │         ││
    ││         │ C-g            clear mark, else cancel without saving    │         ││
    ││         │                                                          │         ││
    ││         │ General                                                  │         ││
    ││         │ ?              toggle this help                          │         ││
    ││         │ C-g / Esc / q  close help                                │         ││
    ││         ╰──────────────────────────────────────────────────────────╯         ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (key (ASCII '?'));
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes ──────────────────────╮╭ Detail <tab> ─────────────────────────────────╮│
    ││> Hello, zet                 ││Hello, zet                                     ││
    ││  OCaml type system          ││#1  note                                       ││
    ││  Morning pages              ││slug: hello-zet                                ││
    ││  Quick capture              ││date: 2026-06-03                               ││
    ││  2026-05-28                 ││                                               ││
    ││                             ││This is the first seeded note. It exists so the││
    ││                             ││TUI has something to render while the data     ││
    ││                             ││layer is wired up.                             ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "e opens editor, C-x C-s saves the body" =
  let handle = notes_handle ~initial_dimensions:{ width = 80; height = 12 } () in
  Bonsai_term_test.send_event handle (key (ASCII 'e'));
  type_string_editor handle " EDITED";
  Handle.show handle;
  [%expect
    {|
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 33) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 34) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 35) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 36) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 37) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 38) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 38) (y 1))) (kind Bar_blinking))))
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes ──────────────────────╮╭ Edit  C-x C-s save  C-g cancel ───────────────╮│
    ││> Hello, zet                 ││EDITEDThis is the first seeded note. It exist  ││
    ││  OCaml type system          ││s so the TUI has something to render while th  ││
    ││  Morning pages              ││e data layer is wired up.                      ││
    ││  Quick capture              ││                                               ││
    ││  2026-05-28                 ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'X'));
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'S'));
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes ──────────────────────╮╭ Detail <tab> ─────────────────────────────────╮│
    ││> Hello, zet                 ││Hello, zet                                     ││
    ││  OCaml type system          ││#1  note                                       ││
    ││  Morning pages              ││slug: hello-zet                                ││
    ││  Quick capture              ││date: 2026-06-03                               ││
    ││  2026-05-28                 ││                                               ││
    ││                             ││EDITEDThis is the first seeded note. It exists ││
    ││                             ││so the TUI has something to render while the   ││
    ││                             ││data layer is wired up.                        ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "C-g cancels an edit without saving" =
  let handle = notes_handle ~initial_dimensions:{ width = 80; height = 12 } () in
  Bonsai_term_test.send_event handle (key (ASCII 'e'));
  type_string_editor handle " THROWAWAY";
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'G'));
  Handle.show handle;
  [%expect
    {|
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 33) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 34) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 35) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 36) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 37) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 38) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 39) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 40) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 41) (y 1))) (kind Bar_blinking))))
    (cursor ())
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes ──────────────────────╮╭ Detail <tab> ─────────────────────────────────╮│
    ││> Hello, zet                 ││Hello, zet                                     ││
    ││  OCaml type system          ││#1  note                                       ││
    ││  Morning pages              ││slug: hello-zet                                ││
    ││  Quick capture              ││date: 2026-06-03                               ││
    ││  2026-05-28                 ││                                               ││
    ││                             ││This is the first seeded note. It exists so the││
    ││                             ││TUI has something to render while the data     ││
    ││                             ││layer is wired up.                             ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "C-Space marks, M-w copies the region, C-y yanks it back" =
  let handle = notes_handle ~initial_dimensions:{ width = 80; height = 12 } () in
  Bonsai_term_test.send_event handle (key (ASCII 'e'));
  Handle.recompute_view handle;
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII '@'));
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes ──────────────────────╮╭ Edit  mark@0  C-x C-e extract  M-w copy ──────╮│
    ││> Hello, zet                 ││This is the first seeded note. It exists so t  ││
    ││  OCaml type system          ││he TUI has something to render while the data  ││
    ││  Morning pages              ││ layer is wired up.                            ││
    ││  Quick capture              ││                                               ││
    ││  2026-05-28                 ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  for _ = 1 to 4 do
    Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'F'));
    Handle.recompute_view handle
  done;
  Bonsai_term_test.send_event handle (key ~mods:[ Meta ] (ASCII 'w'));
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    (cursor (((position ((x 33) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 34) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 35) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 36) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 36) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 36) (y 1))) (kind Bar_blinking))))
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes ──────────────────────╮╭ Edit  C-x C-s save  C-g cancel ───────────────╮│
    ││> Hello, zet                 ││This is the first seeded note. It exists so t  ││
    ││  OCaml type system          ││he TUI has something to render while the data  ││
    ││  Morning pages              ││ layer is wired up.                            ││
    ││  Quick capture              ││                                               ││
    ││  2026-05-28                 ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'Y'));
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    (cursor (((position ((x 40) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 40) (y 1))) (kind Bar_blinking))))
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes ──────────────────────╮╭ Edit  C-x C-s save  C-g cancel ───────────────╮│
    ││> Hello, zet                 ││ThisThis is the first seeded note. It exists   ││
    ││  OCaml type system          ││so the TUI has something to render while the   ││
    ││  Morning pages              ││data layer is wired up.                        ││
    ││  Quick capture              ││                                               ││
    ││  2026-05-28                 ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "C-x C-e extracts a marked region into a new note, atomically" =
  let db, handle =
    notes_handle_with_db ~initial_dimensions:{ width = 80; height = 12 } ()
  in
  (* Open note 1 for editing, mark at offset 0, advance 4 chars so the region is "This". *)
  Bonsai_term_test.send_event handle (key (ASCII 'e'));
  Handle.recompute_view handle;
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII '@'));
  Handle.recompute_view handle;
  for _ = 1 to 4 do
    Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'F'));
    Handle.recompute_view handle
  done;
  (* C-x C-e: slice the region out and enter Extract mode (the editor now holds the title,
     which starts empty). *)
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'X'));
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'E'));
  Handle.recompute_view handle;
  type_string_editor handle "Kakapo";
  Handle.show handle;
  [%expect
    {|
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 33) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 34) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 35) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 36) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 33) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 34) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 35) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 36) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 37) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 38) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 38) (y 1))) (kind Bar_blinking))))
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Related ────────────────────╮╭ Extract  C-x C-s save  C-g cancel ────────────╮│
    ││                             ││Kakapo                                         ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││     (no related notes)      ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  (* C-x C-s commits: new note created + source trimmed, in one transaction. *)
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'X'));
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'S'));
  Handle.recompute_view handle;
  print_endline "-- source note 1 trimmed (leading \"This\" removed) --";
  print_s [%sexp (Option.map (Db.get_by_id db 1) ~f:Db.Note.body : string option)];
  print_endline "-- new note 6 created from the slice --";
  print_s [%sexp (Db.get_by_id db 6 : Db.Note.t option)];
  print_endline "-- new note is FTS-searchable by its slug-derived title --";
  show_titles (Db.search db ~query:"kakapo" ~limit:10 ());
  [%expect
    {|
    (cursor ())
    -- source note 1 trimmed (leading "This" removed) --
    (" is the first seeded note. It exists so the TUI has something to render while the data layer is wired up.")
    -- new note 6 created from the slice --
    ((
      (id 6)
      (slug (kakapo))
      (kind note)
      (title (Kakapo))
      (body This)
      (entry_date ())
      (metadata   ())))
    -- new note is FTS-searchable by its slug-derived title --
    6 kakapo         note    Kakapo
    |}]
;;

let%expect_test "C-x C-e with a title matching an existing note appends to it instead of \
                 creating"
  =
  let db, handle =
    notes_handle_with_db ~initial_dimensions:{ width = 80; height = 12 } ()
  in
  (* Slice "This" out of note 1, then name the extraction "OCaml notes", which slugs to
     "ocaml-notes" — note 2's slug. The Save handler keys the append/create fork on that
     slug match, so the slice appends to note 2 rather than creating a sixth note. *)
  Bonsai_term_test.send_event handle (key (ASCII 'e'));
  Handle.recompute_view handle;
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII '@'));
  Handle.recompute_view handle;
  for _ = 1 to 4 do
    Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'F'));
    Handle.recompute_view handle
  done;
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'X'));
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'E'));
  Handle.recompute_view handle;
  type_string_editor handle "OCaml notes";
  Handle.recompute_view handle;
  (* C-x C-s commits: the slug "ocaml-notes" matches note 2, so the slice appends to it
     and no new note is created. *)
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'X'));
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'S'));
  Handle.recompute_view handle;
  print_endline "-- note 2 body: slice appended after a blank line --";
  print_s [%sexp (Option.map (Db.get_by_id db 2) ~f:Db.Note.body : string option)];
  print_endline "-- source note 1 trimmed --";
  print_s [%sexp (Option.map (Db.get_by_id db 1) ~f:Db.Note.body : string option)];
  print_endline "-- no new note created (corpus still 5) --";
  print_s [%sexp (List.length (Db.list_all db) : int)];
  [%expect
    {|
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 33) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 34) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 35) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 36) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 32) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 33) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 34) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 35) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 36) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 37) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 38) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 39) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 40) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 41) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 42) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 43) (y 1))) (kind Bar_blinking))))
    (cursor (((position ((x 43) (y 1))) (kind Bar_blinking))))
    (cursor ())
    -- note 2 body: slice appended after a blank line --
    ("Notes on the OCaml module system and functors.\n\nThis")
    -- source note 1 trimmed --
    (" is the first seeded note. It exists so the TUI has something to render while the data layer is wired up.")
    -- no new note created (corpus still 5) --
    5
    |}]
;;

let%expect_test "d soft-deletes the selected note; it leaves the corpus but the row \
                 survives"
  =
  let db, handle =
    notes_handle_with_db ~initial_dimensions:{ width = 80; height = 12 } ()
  in
  (* Move to note 2 (OCaml type system) and soft-delete it. [recompute_view] between the
     motion and [d] flushes the cursor update so [d] reads the moved cursor, not a stale 0
     — same batching hazard the chord machine documents. *)
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'N'));
  Handle.recompute_view handle;
  Bonsai_term_test.send_event handle (key (ASCII 'd'));
  Handle.show handle;
  [%expect
    {|
    (cursor ())
    (cursor ())
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │╭ Notes ──────────────────────╮╭ Detail <tab> ─────────────────────────────────╮│
    ││  Hello, zet                 ││Morning pages                                  ││
    ││> Morning pages              ││#3  journal                                    ││
    ││  Quick capture              ││slug: morning-pages                            ││
    ││  2026-05-28                 ││date: 2026-06-02                               ││
    ││                             ││                                               ││
    ││                             ││A journal entry that also mentions OCaml in    ││
    ││                             ││passing.                                       ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    │╰─────────────────────────────╯╰───────────────────────────────────────────────╯│
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  print_endline "-- note 2 still in the table (soft-deleted, not swept) --";
  print_s
    [%sexp
      (Option.bind (Db.get_by_id db 2) ~f:Db.Note.metadata
       |> Option.map ~f:(String.is_substring ~substring:"deleted")
       : bool option)];
  print_endline "-- and hidden from the default corpus --";
  show_titles (Db.list_all db);
  [%expect
    {|
    -- note 2 still in the table (soft-deleted, not swept) --
    (true)
    -- and hidden from the default corpus --
    1 hello-zet      note    Hello, zet
    3 morning-pages  journal Morning pages
    4 untagged-inbox inbox   Quick capture
    5 -              journal 2026-05-28
    |}]
;;

module Model = Zet.Model
module Render = Zet.Render

let browse_model ?(cursor = 0) ?(detail_scroll = 0) () =
  { Model.Model.initial with cursor; detail_scroll }
;;

let search_model ?cursor buf =
  let cursor = Option.value cursor ~default:(String.length buf) in
  { Model.Model.initial with mode = Search; editor = { buf; cursor } }
;;

let apply ?(count = 5) ?(detail_max = 0) model action =
  Model.apply_action_pure ~count ~detail_max model action
;;

let%expect_test "slug_of_title kebab-cases and rejects empty" =
  let show t = print_s [%sexp ((t, Model.slug_of_title t) : string * string option)] in
  show "Kakapo Facts";
  show "  Trailing & weird --- chars!! ";
  show "already-kebab";
  show "ünîcode Tïtle";
  show "";
  show "   ";
  [%expect
    {|
    ("Kakapo Facts" (kakapo-facts))
    ("  Trailing & weird --- chars!! " (trailing-weird-chars))
    (already-kebab (already-kebab))
    ("\195\188n\195\174code T\195\175tle" (unicode-title))
    ("" ())
    ("   " ())
    |}]
;;

let%expect_test "split_region returns slice and remainder, codepoint-correct" =
  let show ~lo ~hi buf =
    print_s [%sexp (Model.split_region ~buf ~lo ~hi : string * string)]
  in
  show ~lo:0 ~hi:5 "Hello, world";
  show ~lo:7 ~hi:12 "Hello, world";
  (* multibyte: 'é' is one codepoint; slicing must not split the byte pair *)
  show ~lo:1 ~hi:4 "aéíou";
  [%expect
    {|
    (Hello ", world")
    (world "Hello, ")
    ("\195\169\195\173o" au)
    |}]
;;

let%expect_test "extract_lines splits by 1-based inclusive line range" =
  let body = "alpha\nbeta\ngamma\ndelta" in
  let show ~lo ~hi =
    print_s [%sexp (Model.extract_lines ~body ~lo ~hi : string * string)]
  in
  print_endline "-- middle range: gap closes cleanly --";
  show ~lo:2 ~hi:3;
  print_endline "-- single line --";
  show ~lo:1 ~hi:1;
  print_endline "-- whole body --";
  show ~lo:1 ~hi:4;
  print_endline "-- last line (remainder keeps no trailing newline) --";
  show ~lo:4 ~hi:4;
  [%expect
    {|
    -- middle range: gap closes cleanly --
    ("beta\ngamma" "alpha\ndelta")
    -- single line --
    (alpha "beta\ngamma\ndelta")
    -- whole body --
    ("alpha\nbeta\ngamma\ndelta" "")
    -- last line (remainder keeps no trailing newline) --
    (delta "alpha\nbeta\ngamma")
    |}]
;;

let%expect_test "extract_lines strips surrounding blank lines from the slice only" =
  let body = "head\n\n  picked  \n\nstill picked\n\ntail" in
  (* lines 2-6 include leading/trailing blanks around the kept content; the slice trims
     outer blank lines but keeps interior ones and per-line indentation. *)
  print_s [%sexp (Model.extract_lines ~body ~lo:2 ~hi:6 : string * string)];
  [%expect {| ("  picked  \n\nstill picked" "head\ntail") |}]
;;

let%expect_test "extract_lines clamps an out-of-range hi to the last line" =
  let body = "one\ntwo\nthree" in
  print_s [%sexp (Model.extract_lines ~body ~lo:2 ~hi:99 : string * string)];
  [%expect {| ("two\nthree" one) |}]
;;

let%expect_test "list cursor clamps at both ends" =
  let m = browse_model ~cursor:0 () in
  print_s [%sexp ((apply m Up).cursor : int)];
  [%expect {| 0 |}];
  print_s [%sexp ((apply m Down).cursor : int)];
  [%expect {| 1 |}];
  let last = apply m Bottom in
  print_s [%sexp (last.cursor : int)];
  [%expect {| 4 |}];
  print_s [%sexp ((apply last Down).cursor : int)];
  [%expect {| 4 |}]
;;

let%expect_test "detail scroll clamps to detail_max when detail focused" =
  let m = { (browse_model ()) with focus = Detail; detail_scroll = 0 } in
  print_s [%sexp ((apply ~detail_max:2 m Up).detail_scroll : int)];
  [%expect {| 0 |}];
  let down = apply ~detail_max:2 m Down in
  print_s [%sexp (down.detail_scroll : int)];
  [%expect {| 1 |}];
  let bottom = apply ~detail_max:2 m Bottom in
  print_s [%sexp (bottom.detail_scroll : int)];
  [%expect {| 2 |}];
  print_s [%sexp ((apply ~detail_max:2 bottom Down).detail_scroll : int)];
  [%expect {| 2 |}];
  print_s [%sexp (bottom.cursor : int)];
  [%expect {| 0 |}]
;;

let%expect_test "start/exit search resets cursor, scroll, and query buffer" =
  let dirty = { (search_model "ocaml") with cursor = 3; detail_scroll = 4 } in
  let started = apply dirty Start_search in
  print_s
    [%sexp
      ((started.mode, started.editor.buf, started.cursor, started.detail_scroll)
       : Model.Mode.t * string * int * int)];
  [%expect {| (Search "" 0 0) |}];
  let exited = apply dirty Exit_search in
  print_s
    [%sexp
      ((exited.mode, exited.editor.buf, exited.cursor, exited.detail_scroll)
       : Model.Mode.t * string * int * int)];
  [%expect {| (Browse "" 0 0) |}]
;;

let%expect_test "query-edit actions" =
  let show (m : Model.Model.t) =
    print_s [%sexp ((m.editor.buf, m.editor.cursor) : string * int)]
  in
  show (apply (search_model "ocaml") (Insert 'x'));
  [%expect {| (ocamlx 6) |}];
  show (apply (search_model "ocaml") Backspace);
  [%expect {| (ocam 4) |}];
  show (apply (search_model ~cursor:2 "ocaml") Delete_forward);
  [%expect {| (ocml 2) |}];
  show (apply (search_model ~cursor:2 "ocaml") Kill_to_end);
  [%expect {| (oc 2) |}];
  show (apply (search_model "type sys") Kill_word_backward);
  [%expect {| ("type " 5) |}];
  show (apply (search_model ~cursor:0 "ocaml") Move_left);
  [%expect {| (ocaml 0) |}];
  show (apply (search_model "ocaml") Move_right);
  [%expect {| (ocaml 5) |}];
  show (apply (search_model ~cursor:2 "ocaml") Move_to_start);
  [%expect {| (ocaml 0) |}];
  show (apply (search_model ~cursor:2 "ocaml") Move_to_end);
  [%expect {| (ocaml 5) |}]
;;

let%expect_test "route maps keys to actions per mode" =
  let route mode event = Model.route { Model.Model.initial with mode } event in
  let show name mode event =
    print_s [%sexp (name : string), (route mode event : Model.Action.t option)]
  in
  show "browse /" Browse (key (ASCII '/'));
  show "browse ?" Browse (key (ASCII '?'));
  show "browse C-n" Browse (key ~mods:[ Ctrl ] (ASCII 'N'));
  show "browse Tab" Browse (key Tab);
  show "browse x (unbound)" Browse (key (ASCII 'x'));
  [%expect
    {|
    ("browse /" (Start_search))
    ("browse ?" (Open_help))
    ("browse C-n" (Down))
    ("browse Tab" (Toggle_focus))
    ("browse x (unbound)" ())
    |}];
  show "search a" Search (key (ASCII 'a'));
  show "search Esc" Search (key Escape);
  show "search Enter" Search (key Enter);
  show "search C-w" Search (key ~mods:[ Ctrl ] (ASCII 'W'));
  [%expect
    {|
    ("search a" ((Insert a)))
    ("search Esc" (Exit_search))
    ("search Enter" (Toggle_focus))
    ("search C-w" (Kill_word_backward))
    |}];
  show "help q" Help (key (ASCII 'q'));
  show "help Esc" Help (key Escape);
  show "help x" Help (key (ASCII 'x'));
  [%expect
    {|
    ("help q" (Close_help))
    ("help Esc" (Close_help))
    ("help x" ())
    |}];
  show "edit C-n" Edit (key ~mods:[ Ctrl ] (ASCII 'N'));
  show "edit a" Edit (key (ASCII 'a'));
  [%expect {|
    ("edit C-n" ())
    ("edit a" ())
    |}]
;;

let%expect_test "wrap_line wraps, hard-splits, and preserves blanks" =
  let show line = print_s [%sexp (Render.wrap_line ~width:6 line : string list)] in
  show "the quick brown fox";
  [%expect {| (the quick brown fox) |}];
  show "abcdefghij";
  [%expect {| (abcdef ghij) |}];
  show "";
  [%expect {| ("") |}]
;;

let%expect_test "detail_max_scroll matches the wrapped-body geometry" =
  let note : Db.Note.t =
    { id = 1
    ; slug = Some "s"
    ; kind = "note"
    ; title = Some "t"
    ; body = "one two three four five six seven eight"
    ; entry_date = Some "2026-06-07"
    ; metadata = None
    }
  in
  let lines = List.length (Render.detail_body_lines ~width:6 note.body) in
  print_s [%sexp "wrapped body lines", (lines : int)];
  [%expect {| ("wrapped body lines" 8) |}];
  print_s [%sexp (Render.detail_max_scroll ~width:6 ~height:9 (Some note) : int)];
  [%expect {| 4 |}];
  print_s [%sexp (Render.detail_max_scroll ~width:6 ~height:1 (Some note) : int)];
  [%expect {| 8 |}];
  print_s [%sexp (Render.detail_max_scroll ~width:6 ~height:9 None : int)];
  [%expect {| 0 |}]
;;
