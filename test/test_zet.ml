open! Core
open Bonsai_test
open Bonsai_term
module Db = Zet.Db

let key ?(mods = []) k = Event.Key_press { key = k; mods }

(* Build an in-memory DB seeded from the canonical schema + seed scripts so the tests are
   hermetic and exercise the real SQL the app ships. *)
let seeded_db () =
  let db = Db.open_ ":memory:" in
  Db.exec_script db (In_channel.read_all "../db/schema.sql");
  Db.exec_script db (In_channel.read_all "../db/seed.sql");
  db
;;

(* Render a note list compactly so query tests assert on what matters (which notes, in
   what order) without pinning every column. *)
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
  Zet.print_notes notes;
  [%expect
    {|
    2	ocaml-notes	note	OCaml type system
    3	morning-pages	journal	Morning pages
    |}]
;;

let notes_handle ?initial_dimensions () =
  let db = seeded_db () in
  let notes = Db.list_all db in
  Db.close db;
  Bonsai_term_test.create_handle ?initial_dimensions (Zet.app ~notes)
;;

let%expect_test "app renders two panes with list focused, first note selected" =
  let handle = notes_handle () in
  Handle.show handle;
  [%expect
    {|
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

(* C-n moves the cursor down; Tab moves focus to the detail pane (title hints flip); the
   detail pane tracks the selected note's body. *)
let%expect_test "C-n moves cursor, Tab switches focus to detail" =
  let handle = notes_handle () in
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'N'));
  Bonsai_term_test.send_event handle (key Tab);
  Handle.show handle;
  [%expect
    {|
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

(* In a short terminal the selected note's body overflows the detail pane. With the detail
   pane focused, C-n scrolls the body: the first body lines drop off the top while the
   fixed header stays put, and later wrapped lines scroll into view. *)
let%expect_test "detail pane scrolls its body when focused" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key Tab);
  Handle.show handle;
  [%expect {|
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
  [%expect {|
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
