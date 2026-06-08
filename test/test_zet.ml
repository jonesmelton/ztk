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

(* [update_body] overwrites the body in place; the [notes_au] trigger reindexes FTS so the
   new text is searchable and the old text is not. *)
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

(* [resolve_note] backs the [show]/[edit] subcommands' IDENT argument: numeric strings hit
   by id, non-numeric by slug, and a numeric string with no id match falls back to slug. *)
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

(* The [edit] subcommand composes [resolve_note] + [update_body]: resolve an IDENT, then
   overwrite that note's body. This is the in-process core of [zet edit IDENT]. *)
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

(* The handle keeps the seeded DB open for its lifetime: [Zet.App.app] reads the corpus up
   front but also re-queries it live for search, so the connection must stay alive. Tests
   are short-lived, so we don't bother closing it. *)
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

(* C-n moves the cursor down; Tab moves focus to the detail pane (title hints flip); the
   detail pane tracks the selected note's body. *)
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

(* In a short terminal the selected note's body overflows the detail pane. With the detail
   pane focused, C-n scrolls the body: the first body lines drop off the top while the
   fixed header stays put, and later wrapped lines scroll into view. *)
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

(* Type each char of [s] into a handle, one Insert per ASCII byte. *)
let type_string handle s =
  String.iter s ~f:(fun c -> Bonsai_term_test.send_event handle (key (ASCII c)))
;;

(* Like [type_string], but recomputes a frame between keystrokes. The text editor's insert
   reads-then-writes its buffer, so several inserts dispatched within one stabilization
   collapse onto the same pre-burst state and all but one are lost. A live terminal
   renders (and so stabilizes) between keys; this mimics that so editor input lands in
   order. *)
let type_string_editor handle s =
  String.iter s ~f:(fun c ->
    Bonsai_term_test.send_event handle (key (ASCII c));
    Handle.recompute_view handle)
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

(* Typing a matching query shows ranked FTS results in the list pane; the title echoes the
   query. "ocaml" matches the two seeded notes that mention OCaml. *)
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

(* A query that matches nothing shows the (no matches) placeholder, distinct from browse's
   (no notes). *)
let%expect_test "non-matching query shows (no matches)" =
  let handle = notes_handle ~initial_dimensions:{ width = 54; height = 10 } () in
  Bonsai_term_test.send_event handle (key (ASCII '/'));
  type_string handle "zzzznope";
  Handle.show handle;
  [%expect
    {|
    (cursor (((position ((x 22) (y 1))) (kind Bar_blinking))))
    ┌──────────────────────────────────────────────────────┐
    │╭ Notes ────────────╮╭ Edit  C-x C-s save  C-g cancel │
    ││> Hello, zet       ││This is the first seeded note  ││
    ││  OCaml type system││. It exists so the TUI has so  ││
    ││  Morning pages    ││mething to render while the d  ││
    ││  Quick capture    ││ata layer is wired up.         ││
    ││  2026-05-28       ││                               ││
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

(* An ANSI-rendering handle: [Handle.show] on it surfaces color/style escapes (visualized
   as markers) instead of stripping them, so highlight spans are visible to the snapshot.
   The dumb-cap [notes_handle] above can't see them. *)
let ansi_handle ?initial_dimensions () =
  let db = seeded_db () in
  Bonsai_term_test.create_handle
    ?initial_dimensions
    ~capability:Bonsai_term_test.Capability.Ansi
    (Zet.App.app ~db)
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

(* [?] in Browse opens the keybinding overlay centered over the panes; the corpus stays
   visible behind it. [?] again (or Esc / C-g / q) dismisses it back to Browse. *)
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

(* [e] opens the editor in the detail pane, seeded with the selected note's body. Typing
   appends to it; [C-x C-s] writes the body to the DB, reloads the corpus, and returns to
   Browse — the detail pane then shows the persisted text. The terminal cursor is reported
   only while editing (the [(cursor ...)] line). *)
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
  (* Save: C-x then C-s with NO frame between them. The chord's armed-bit lives in a state
     machine read inside apply_action, so the second key sees the first key's effect even
     when they're dispatched back-to-back (the batching case that broke the old
     closure-based chord). Returns to Browse; the detail pane shows the persisted body,
     read back from the reloaded corpus. *)
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

(* [C-g] in Edit mode discards: the body is not written, and the detail pane shows the
   original text again. *)
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

(* Mark + copy-region (in-model kill ring): [C-Space] sets the mark at the caret — the
   Edit title then shows [mark@N]. Moving the caret right with [C-f] extends the region;
   [M-w] copies [mark, caret] into the kill ring and clears the mark (title reverts).
   [C-y] yanks the ring at the caret, so the copied text appears a second time in the
   body. This proves the slice contents end-to-end without exposing the model. *)
let%expect_test "C-Space marks, M-w copies the region, C-y yanks it back" =
  let handle = notes_handle ~initial_dimensions:{ width = 80; height = 12 } () in
  Bonsai_term_test.send_event handle (key (ASCII 'e'));
  Handle.recompute_view handle;
  (* Caret at 0. Set the mark there; the title should read mark@0. C-Space arrives as C-@
     (control code 0x00) on the terminals we target. *)
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
  (* Extend the region to [0,4] = "This" by moving the caret right four chars, then copy.
     The mark clears, so the title reverts to the default save/cancel hint. *)
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
  (* Yank at the caret (position 4): "This" is inserted, so the line opens "ThisThis...". *)
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

(* ── Pure-core unit tests ─────────────────────────────────────────────────────────────
   These drive the modules the split isolated — [Model] (reducer + routing), [Render]
   (layout helpers), [Fts_query] — directly, without going through a rendered frame. They
   pin behavior that the app-level snapshots above only exercise transitively. *)

module Model = Zet.Model
module Render = Zet.Render

(* A model in Browse with the list focused at [cursor]; the rest is initial. *)
let browse_model ?(cursor = 0) ?(detail_scroll = 0) () =
  { Model.Model.initial with cursor; detail_scroll }
;;

(* A model in Search whose query buffer is [buf] with the edit cursor at [cursor]
   (defaulting to end of buffer). *)
let search_model ?cursor buf =
  let cursor = Option.value cursor ~default:(String.length buf) in
  { Model.Model.initial with mode = Search; editor = { buf; cursor } }
;;

let apply ?(count = 5) ?(detail_max = 0) model action =
  Model.apply_action_pure ~count ~detail_max model action
;;

let%expect_test "list cursor clamps at both ends" =
  let m = browse_model ~cursor:0 () in
  (* Up from the top stays at 0. *)
  print_s [%sexp ((apply m Up).cursor : int)];
  [%expect {| 0 |}];
  (* Down from the top advances. *)
  print_s [%sexp ((apply m Down).cursor : int)];
  [%expect {| 1 |}];
  (* Bottom goes to count-1; a further Down can't exceed it. *)
  let last = apply m Bottom in
  print_s [%sexp (last.cursor : int)];
  [%expect {| 4 |}];
  print_s [%sexp ((apply last Down).cursor : int)];
  [%expect {| 4 |}]
;;

(* With the detail pane focused, the nav keys scroll the body and clamp to [detail_max]
   rather than moving the list cursor. *)
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
  (* The list cursor is untouched while detail is focused. *)
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

(* Each query-edit action against the buffer "ocaml". Reports buf + edit cursor so insert,
   delete, kill, and motion are all pinned in one place. *)
let%expect_test "query-edit actions" =
  let show (m : Model.Model.t) =
    print_s [%sexp ((m.editor.buf, m.editor.cursor) : string * int)]
  in
  (* Insert at end. *)
  show (apply (search_model "ocaml") (Insert 'x'));
  [%expect {| (ocamlx 6) |}];
  (* Backspace from end. *)
  show (apply (search_model "ocaml") Backspace);
  [%expect {| (ocam 4) |}];
  (* Delete_forward in the middle (cursor at 2 deletes 'a'). *)
  show (apply (search_model ~cursor:2 "ocaml") Delete_forward);
  [%expect {| (ocml 2) |}];
  (* Kill_to_end from cursor 2. *)
  show (apply (search_model ~cursor:2 "ocaml") Kill_to_end);
  [%expect {| (oc 2) |}];
  (* Kill_word_backward over "type sys" at end deletes the last word. *)
  show (apply (search_model "type sys") Kill_word_backward);
  [%expect {| ("type " 5) |}];
  (* Motion: left/right move the edit cursor and clamp. *)
  show (apply (search_model ~cursor:0 "ocaml") Move_left);
  [%expect {| (ocaml 0) |}];
  show (apply (search_model "ocaml") Move_right);
  [%expect {| (ocaml 5) |}];
  show (apply (search_model ~cursor:2 "ocaml") Move_to_start);
  [%expect {| (ocaml 0) |}];
  show (apply (search_model ~cursor:2 "ocaml") Move_to_end);
  [%expect {| (ocaml 5) |}]
;;

(* The routing matrix: for each mode, a representative set of keys mapped to their action
   (or none). This is the contract that today lives only in [Model.route]'s prose. *)
let%expect_test "route maps keys to actions per mode" =
  let route mode event = Model.route { Model.Model.initial with mode } event in
  let show name mode event =
    print_s [%sexp (name : string), (route mode event : Model.Action.t option)]
  in
  (* Browse. *)
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
  (* Search captures text + emacs editing. *)
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
  (* Help swallows everything except the dismiss keys. *)
  show "help q" Help (key (ASCII 'q'));
  show "help Esc" Help (key Escape);
  show "help x" Help (key (ASCII 'x'));
  [%expect
    {|
    ("help q" (Close_help))
    ("help Esc" (Close_help))
    ("help x" ())
    |}];
  (* Edit routes nothing through the reducer — the handler + chord own it. *)
  show "edit C-n" Edit (key ~mods:[ Ctrl ] (ASCII 'N'));
  show "edit a" Edit (key (ASCII 'a'));
  [%expect {|
    ("edit C-n" ())
    ("edit a" ())
    |}]
;;

let%expect_test "wrap_line wraps, hard-splits, and preserves blanks" =
  let show line = print_s [%sexp (Render.wrap_line ~width:6 line : string list)] in
  (* Greedy word wrap. *)
  show "the quick brown fox";
  [%expect {| (the quick brown fox) |}];
  (* A word longer than the width is hard-split into width-sized chunks. *)
  show "abcdefghij";
  [%expect {| (abcdef ghij) |}];
  (* A blank line stays one blank line. *)
  show "";
  [%expect {| ("") |}]
;;

(* [detail_max_scroll] must equal (wrapped body lines) - (rows under the header), floored
   at 0 — the same geometry the renderer slices by, so a scroll offset can't run past the
   body. *)
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
  (* width 6 wraps the body; header for this note is title+#id/kind+slug+date+blank = 5. *)
  let lines = List.length (Render.detail_body_lines ~width:6 note.body) in
  print_s [%sexp "wrapped body lines", (lines : int)];
  [%expect {| ("wrapped body lines" 8) |}];
  (* height 9 → 9-5 = 4 body rows → max scroll 8-4 = 4. *)
  print_s [%sexp (Render.detail_max_scroll ~width:6 ~height:9 (Some note) : int)];
  [%expect {| 4 |}];
  (* Tiny height → header eats everything → 0 body rows → max scroll = all lines. *)
  print_s [%sexp (Render.detail_max_scroll ~width:6 ~height:1 (Some note) : int)];
  [%expect {| 8 |}];
  (* No note → 0. *)
  print_s [%sexp (Render.detail_max_scroll ~width:6 ~height:9 None : int)];
  [%expect {| 0 |}]
;;
