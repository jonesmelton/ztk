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
    ││  Morning pages              ││slug: hello-zet                                ││
    ││  Quick capture              ││date: 2026-06-03                               ││
    ││  2026-05-28    ╭ Keybindings ───────────────────────────────╮                ││
    ││                │ Navigation                                 │It exists so the││
    ││                │ C-n / C-p      next / previous note        │le the data     ││
    ││                │ C-a / C-e      first / last note           │                ││
    ││                │ Tab            switch list / detail pane   │                ││
    ││                │ Enter          focus the detail pane       │                ││
    ││                │                                            │                ││
    ││                │ Detail pane                                │                ││
    ││                │ C-n / C-p      scroll body down / up       │                ││
    ││                │ C-a / C-e      top / bottom                │                ││
    ││                │                                            │                ││
    ││                │ Search                                     │                ││
    ││                │ /              start full-text search      │                ││
    ││                │ C-b / C-f      move cursor left / right    │                ││
    ││                │ C-k            kill to end of line         │                ││
    ││                │ C-w            kill word backward          │                ││
    ││                │ C-d / DEL      delete char forward / back  │                ││
    ││                │ Enter          commit query, focus results │                ││
    ││                │ Esc            cancel search               │                ││
    ││                │                                            │                ││
    ││                │ Editing                                    │                ││
    ││                │ e              edit selected note's body   │                ││
    ││                │ C-x C-s        save changes                │                ││
    ││                │ C-g            cancel without saving       │                ││
    ││                │                                            │                ││
    ││                │ General                                    │                ││
    ││                │ ?              toggle this help            │                ││
    ││                │ C-g / Esc / q  close help                  │                ││
    ││                ╰────────────────────────────────────────────╯                ││
    ││                             ││                                               ││
    ││                             ││                                               ││
    ││                             ││                                               ││
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
    │╭ Notes ──────────────────────╮╭ Edit  mark@0  M-w copy  C-g clear ────────────╮│
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
