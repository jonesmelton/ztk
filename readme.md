# zet

A tui wrapper around my personal sqlite db that I use for technical notes and developer journal.

This is filling a real need for me but also largely just exploring some technologies and techniques:

[OxCaml](https://oxcaml.org) fork of OCaml, though so far I'm not using any of its interesting features.
[`bonsai_term`](https://github.com/janestreet/bonsai_term) an Elm architecture/mvu type tui framework.

Bonsai's approach to state and incremental UI updates lets you sequence events directly in tests and then assert against the text output by the view at that point.

Combined with snapshot testing you get the most straightforward testing possible for a tui app. Basically just saving screenshots of your app.


## Develop

Requires the `5.2.0+ox` opam switch. Common tasks are in a `justfile`
([`just`](https://github.com/casey/just) — `brew install just`); `just` with no
args lists them.

| recipe | dune command |
|---|---|
| `just build` | `dune build` |
| `just run` | `dune exec bin/main.exe` |
| `just zet <args>` | `dune exec bin/main.exe -- <args>` |
| `just test` | `dune runtest` |
| `just test-force` | `dune runtest --force` |
| `just promote` | `dune runtest --auto-promote` |
| `just fmt` | `dune build @fmt --auto-promote` |

## Testing

Snapshot tests live in `test/test_zet.ml`. Each test sequences events against an
in-memory SQLite database seeded from `db/schema.sql` + `db/seed.sql`, then
asserts on the rendered ASCII frame. A representative snapshot:

```ocaml
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
    ...
    |}]
```

Events are dispatched through the real handler — `send_event handle (key (ASCII '/'))` — the Bonsai graph stabilizes, and `Handle.show` renders the resulting `View.t` to ASCII. `dune runtest --auto-promote` accepts new or changed output.

## Run

`just run` and bare `dune exec bin/main.exe` both use the default db path:
`$ZET_DB` if set, else `~/.zet/zet.db`, else `zet.db` in the working directory.
Pass `-db PATH` to override. `just zet <args>` runs a headless subcommand
(e.g. `just zet search foo`).

