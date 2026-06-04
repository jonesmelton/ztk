# CLAUDE.md — zet

A terminal UI over a personal zettelkasten (SQLite). Built on `bonsai_term`
(OxCaml). Read `docs/plan.md` for the build order and `docs/decisions.md` for the why.

## What this project is

Read-mostly TUI for searching/browsing notes + editing metadata + import/export.
Note *authoring* happens elsewhere. v1 is bounded by the existing SQLite schema
(see `docs/decisions.md`); **links/backlinks are deferred** — there is no edges table.

## Toolchain

- opam switch: **`5.2.0+ox`** (OxCaml). `core`, `async`, `bonsai`, `bonsai_term`,
  `bonsai_term_test`, `bonsai_term_components` installed from the `ox` opam repo
  at `v0.18~preview.130.91+190`. `ocaml`/`dune` from `~/.opam/5.2.0+ox/bin`.
- **Version skew:** the `references/` clones are on *newer/other* revisions
  (`bonsai_term*` at `130.100+614`, `bonsai_term_examples` on `with-extensions`
  at `130.83+317`) than the installed libs (`130.91+190`), and `130.100+614`
  isn't in opam. So `references/` is **approximate** API docs — when it disagrees
  with the compiler, the installed `.mli` wins. Read installed interfaces at
  `~/.opam/5.2.0+ox/lib/<pkg>/*.mli`.
- SQLite: planned via `caqti` + `caqti-async` + `caqti-driver-sqlite3`
  (**not yet installed** — add + verify it builds on the OxCaml switch).
- `sqlite3` CLI at `/opt/homebrew/opt/sqlite/bin/sqlite3` (handy for fixtures).
- Shell is **fish**. Each `Bash` call is a fresh non-interactive shell with no
  persisted `cwd` — **use absolute paths**; don't rely on a prior `cd`.

## Layout (planned)

```
src/   library `zet`        (app, model, view, db)
bin/   executable           (Command_unix.run Zet.command)
test/  expect/snapshot tests
references/   cloned JS repos, GITIGNORED — read-only API reference
```

## Reference repos (in `references/`, gitignored)

- `bonsai_term` — core. Read `src/bonsai_term.mli`, `src/view.mli`,
  `src/event.mli` for the API.
- `bonsai_term_examples/hello_world` — minimal app template (dune conventions).
- `bonsai_term_examples/ncdu` — navigable weighted tree via a `Make(K)(W)` functor.
- `bonsai_term_components` — reusable widgets: `virtual_list`, `scroller`,
  `textbox`, `text_editor`, `border_box`, `less_keybindings`, `bindings`,
  `click_handler`, `typography`, `catppuccin`.
- **`strace_ui` — THE structural template.** Two-pane list+detail app with a
  filter editor and manual focus routing. Lift `virtual_list.ml`,
  `filter_editor.ml`, and the focus-routing skeleton in `strace_ui_app.ml`.
- `proctopus` — another full app for reference.

## bonsai_term essentials

- An app is:
  ```ocaml
  val app
    :  dimensions:Dimensions.t Bonsai.t
    -> local_ Bonsai.graph
    -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  ```
  Run with `Bonsai_term.start app`.
- `View.t` is immutable image combinators: `text`, `vcat`, `hcat`, `zcat`,
  `pad`, `crop`, `center ~within`, `rectangle`, `with_colors`. notty underneath.
- `Event.t` = `Key_press {key; mods}` | `Mouse {kind; position; mods}` | `Paste`.
- **No focus management exists** — you track `focus` in the model and route every
  key by hand in the single global handler. (strace_ui: `match model.focus with`.)
- Mouse hit-testing is done via the `Tag` API: tag a view region, look it up by
  position on click.
- Ppx set: `(pps ppx_jane bonsai.ppx_bonsai)`. Syntax uses `let%arr`,
  `match%sub`, `let%map_open.Command`.

### Performance gotcha (from `virtual_list.mli`)
Bonsai cuts off recomputation by `phys_equal`. If you store a mutable
collection, **wrap it in an immutable box and create a new box on mutation** so
the cutoff sees a change. Conversely, *don't* needlessly reallocate values you
want cut off. This matters for a large note corpus.

## Testing (the whole point of this stack)

Snapshot/expect tests via `bonsai_term_test`. ⚠️ The installed `130.91+190` has
**no `print_view` / `last_view`** (those are in the newer `references/` clone).
Render with `Handle.show` from `Bonsai_test` instead:
```ocaml
open! Core
open Bonsai_test                 (* Handle.show *)
open Bonsai_term                 (* Event.t and its constructors *)

let%expect_test "search opens" =
  let handle = Bonsai_term_test.create_handle Zet.app in
  Bonsai_term_test.send_event
    handle
    (Event.Key_press { key = Event.Key.ASCII '/'; mods = [] });
  Handle.show handle;
  [%expect {| ...ascii screenshot... |}]
;;
```
`test/dune` libs: `zet bonsai bonsai_term bonsai_term_test bonsai_test core`,
with `(inline_tests)` + `(pps ppx_jane bonsai.ppx_bonsai)`.
- `dune runtest` diffs; `dune runtest --auto-promote` rewrites expected output
  (first auto-promote run exits 1 as it writes the diff; re-run is green).
- Drive interaction with `send_event` / `do_actions` / `set_dimensions`.
- **Write the failing snapshot test before behavioral changes** where feasible
  (user preference). Snapshots verify *layout + logic*, NOT scroll feel or render
  perf — flag those for the user to check manually; don't claim "works" for feel.

## Workflow conventions

- Build: `dune build`. Test: `dune runtest`. Run: the built exe in `bin/`.
- Keep `references/` gitignored; never edit it (it's read-only upstream source).
- Commit only when asked. This dir is not yet a git repo — `git init` if/when
  the user wants version control.
- Diagnose root causes; no `--no-verify` / `--force` / disable-the-check moves.
