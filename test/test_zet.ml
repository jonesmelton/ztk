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

(* The handle keeps the seeded DB open for its lifetime: [Zet.app] reads the corpus up
   front but also re-queries it live for search, so the connection must stay alive. Tests
   are short-lived, so we don't bother closing it. *)
let notes_handle ?initial_dimensions () =
  let db = seeded_db () in
  Bonsai_term_test.create_handle ?initial_dimensions (Zet.app ~db)
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
  [%expect
    {|
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

(* Type each char of [s] into a handle, one Insert per ASCII byte. *)
let type_string handle s =
  String.iter s ~f:(fun c -> Bonsai_term_test.send_event handle (key (ASCII c)))
;;

(* The sanitizer maps arbitrary input to a valid FTS5 MATCH: barewords become quoted
   prefix phrases, FTS5-special chars are stripped, and whitespace/empty collapses to "". *)
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

(* [/] enters search mode: the list pane title flips to the query prompt with a cursor
   marker, and with an empty query the result list is empty (awaiting input). *)
let%expect_test "slash enters search mode with an empty query" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  Handle.show handle;
  [%expect
    {|
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

(* Typing a matching query shows ranked FTS results in the list pane; the title echoes the
   query. "ocaml" matches the two seeded notes that mention OCaml. *)
let%expect_test "typing a matching query shows ranked results" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "ocaml";
  Handle.show handle;
  [%expect
    {|
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

(* A query that matches nothing shows the (no matches) placeholder, distinct from browse's
   (no notes). *)
let%expect_test "non-matching query shows (no matches)" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "zzzznope";
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────┐
    │╭ Search: zznope▏ ──╮╭ Detail <tab> ─────────────────╮│
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

(* Backspace shortens the query and the result set re-derives; the title reflects the edit
   cursor position. *)
let%expect_test "backspace shortens the query and updates results" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "ocamlx";
  Bonsai_term_test.send_event handle (key Backspace);
  Handle.show handle;
  [%expect
    {|
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

(* Esc leaves search mode: the list returns to the full browse corpus and the title is
   back to "Notes", cursor reset to the top. *)
let%expect_test "escape exits search back to the browse corpus" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "ocaml";
  Bonsai_term_test.send_event handle (key Escape);
  Handle.show handle;
  [%expect
    {|
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

(* After typing a query, Enter hands focus to the result list; C-n moves the selection
   over results and the detail pane tracks the selected match. Cross-pane behavior is
   unchanged from browse. *)
let%expect_test "select a result and view it in the detail pane" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 12 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "ocaml";
  Bonsai_term_test.send_event handle (key Enter);
  Bonsai_term_test.send_event handle (key ~mods:[ Ctrl ] (ASCII 'N'));
  Handle.show handle;
  [%expect
    {|
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

(* Regression guard for the sanitizer: a half-typed/garbage query (lone quote, then a bare
   dash, then a trailing star) must not raise an FTS5 syntax error — the app keeps
   rendering a valid (empty) result state throughout. *)
let%expect_test "garbage query does not crash the search" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "\"-*";
  Handle.show handle;
  [%expect
    {|
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

(* An ANSI-rendering handle: [Handle.show] on it surfaces color/style escapes (visualized
   as markers) instead of stripping them, so highlight spans are visible to the snapshot.
   The dumb-cap [notes_handle] above can't see them. *)
let ansi_handle ?initial_dimensions () =
  let db = seeded_db () in
  Bonsai_term_test.create_handle
    ?initial_dimensions
    ~capability:Bonsai_term_test.Capability.Ansi
    (Zet.app ~db)
;;

(* Matched query terms are emphasized wherever they appear, by prefix and
   case-insensitively: "oc" highlights "OCaml" in both the list row and the detail header.
   Rendered with the ANSI cap so the styled runs show up as escape markers. *)
let%expect_test "search highlights matched terms in list and detail" =
  let handle = ansi_handle ~initial_dimensions:{ width = 54; height = 9 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "oc";
  Handle.show handle;
  [%expect
    {|
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

(* [?] in Browse opens the keybinding overlay centered over the panes; the corpus stays
   visible behind it. [?] again (or Esc / C-g / q) dismisses it back to Browse. *)
let%expect_test "? opens and closes the help overlay" =
  let handle = notes_handle () in
  Bonsai_term_test.send_event handle (key (ASCII '?'));
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
    ││                ╭ Keybindings ───────────────────────────────╮                ││
    ││                │ Navigation                                 │                ││
    ││                │ C-n / C-p      next / previous note        │                ││
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
    ││                │ General                                    │                ││
    ││                │ ?              toggle this help            │                ││
    ││                │ C-g / Esc / q  close help                  │                ││
    ││                ╰────────────────────────────────────────────╯                ││
    ││                             ││                                               ││
    ││                             ││                                               ││
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
