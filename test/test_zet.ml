open! Core
open Bonsai_test
module Db = Zet.Db

(* Build an in-memory DB seeded from the canonical schema + seed scripts so the
   tests are hermetic and exercise the real SQL the app ships. *)
let seeded_db () =
  let db = Db.open_ ":memory:" in
  Db.exec_script db (In_channel.read_all "../db/schema.sql");
  Db.exec_script db (In_channel.read_all "../db/seed.sql");
  db
;;

let%expect_test "list_all returns the seeded note" =
  let db = seeded_db () in
  let notes = Db.list_all db in
  Db.close db;
  print_s [%sexp (notes : Db.Note.t list)];
  [%expect
    {|
    ((
      (id    1)
      (slug  hello-zet)
      (kind  note)
      (title "Hello, zet")
      (body
       "This is the first seeded note. It exists so the TUI has something to render while the data layer is wired up.")
      (entry_date (2026-06-03))
      (metadata   ("{\"tags\":[\"seed\",\"demo\"]}"))))
    |}]
;;

let%expect_test "app renders seeded notes" =
  let db = seeded_db () in
  let notes = Db.list_all db in
  Db.close db;
  let handle = Bonsai_term_test.create_handle (Zet.app ~notes) in
  Handle.show handle;
  [%expect {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                 1  Hello, zet                                  │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
