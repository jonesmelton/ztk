# zet — implementation plan

A terminal UI for searching, browsing, and lightly editing metadata over an
existing personal zettelkasten stored in SQLite. Built on
[`bonsai_term`](https://github.com/janestreet/bonsai_term) (OxCaml).

This plan is the step-by-step build order. For *why* the decisions were made,
see `decisions.md`. For working conventions, see `CLAUDE.md`.

---

## Status / context

- **Phase 0 complete** (2026-06-03). Project scaffolded, `dune build` and
  `dune runtest` green on `5.2.0+ox`, binary builds. See "Phase 0 — DONE" below.
- **Phase 1 complete** (2026-06-04). Typed sqlite3_utils data layer. See below.
- **Phase 2 complete** (2026-06-04). Read-only two-pane browse UI + headless CLI
  mirrors (`list`/`show`/`search`). See "Phase 2 — DONE" below.
- Toolchain: opam switch `5.2.0+ox` (OxCaml), `core` / `async` /
  `ppx_jane` at `v0.18~preview.130.91+190`. `sqlite3` CLI present at
  `/opt/homebrew/opt/sqlite/bin/sqlite3`.
- The **bonsai_term stack is installed from the `ox` opam repo** at
  `v0.18~preview.130.91+190` (matching `core`/`async`):
  `bonsai bonsai_term bonsai_term_test bonsai_term_components` (pulls
  `virtual_dom`, `notty-community`, `notty_async`, … transitively). This was
  *not* preinstalled despite earlier notes claiming so — installed during Phase 0.
- **Version skew vs. `references/` — important.** The opam libs we build against
  are `130.91+190`. The cloned reference repos are on *newer/different*
  revisions, so their APIs do **not** all match what's installed:
  - `bonsai_term` / `bonsai_term_test` / `bonsai_term_components` clones are at
    `130.100+614` (ahead of opam). `bonsai_term`'s `oxcaml` branch points at the
    same commit as `master`, so switching branches does not close the gap.
  - `bonsai_term_examples` clone is on branch `with-extensions` at `130.83+317`
    (a *third*, older revision).
  - opam only publishes up to `130.91+190`; `130.100+614` is **not** available
    there, and the whole JS suite releases in lockstep (matching it would mean
    source-pinning core/async/bonsai/virtual_dom/… — not worth it for v1).
  - **Consequence:** treat `references/` as *approximate* API docs. When the
    compiler disagrees with a reference signature, the *installed* `.mli` wins.
    Read installed interfaces at `~/.opam/5.2.0+ox/lib/<pkg>/*.mli`.
  - Concrete bite already hit: `Bonsai_term_test.print_view` / `last_view` exist
    in the `130.100+614` clone but **not** in installed `130.91+190`. The working
    snapshot idiom is `open Bonsai_test` + `Handle.show handle` (see Phase 0).
- **Data layer uses synchronous `sqlite3_utils`** (over the `sqlite3` bindings;
  both installed). The planned `caqti-async` was dropped — see Phase 1 below and
  `decisions.md`. `caqti` is *not* installed.
- Reference repos cloned under `references/` (gitignored): `bonsai_term`,
  `bonsai_term_examples`, `bonsai_term_components`, `bonsai_term_test`,
  `strace_ui`, `proctopus`. **`strace_ui` is the structural template** — it is a
  real two-pane (list + detail) bonsai_term app with a filter editor and manual
  focus routing, which is almost exactly zet's shape.
- DB schema is known (see `decisions.md`). Single-writer assumption (the TUI and
  any external editor are never writing concurrently).

---

## Build order

### Phase 0 — Hello-world spike (toolchain proof) — DONE ✅ (2026-06-03)

Goal: prove the `bonsai_term` build + snapshot-test loop works on this OxCaml
switch before designing anything further. **No database in this phase.**

**Outcome:** all gates green except the live-TTY check (deferred to the human —
the agent harness can't drive an interactive terminal). What shipped:
`dune-project`, root `dune` (`(dirs (:standard \ references))` so dune doesn't
descend into the nested-`dune-project` reference clones), `src/{dune,zet.ml,
zet.mli}`, `bin/{dune,main.ml,main.mli}`, `test/{dune,test_zet.ml}`. The snapshot
renders "hello zet" centered in an 80×40 frame. Two deviations from the recipe
below were forced by the installed library version — see the ⚠️ notes inline.

1. **Scaffold the dune project.** Mirror the `hello_world` example layout
   (`references/bonsai_term_examples/hello_world`):
   - `dune-project`: `(lang dune 3.17)`
   - `src/dune` library `zet`, libraries `async bonsai bonsai_term core`,
     preprocess `(pps ppx_jane bonsai.ppx_bonsai)`.
   - `bin/dune` executable, `core_unix.command_unix`, `(modes byte exe)`.
   - `src/zet.ml`: the minimal app. ⚠️ In the `.mli` the graph param is written
     `Bonsai.graph @ local` (OxCaml mode syntax, as in the `hello_world`
     example); in the `.ml` the binder is `(local_ _graph)`:
     ```ocaml
     (* zet.mli *)
     val app
       :  dimensions:Dimensions.t Bonsai.t
       -> Bonsai.graph @ local
       -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
     ```
     Render `View.center ~within:dimensions (View.text "hello zet")`, handler
     = `Bonsai.return (fun _ -> Effect.Ignore)`. The `command` uses
     `Async.Command.async_or_error` wrapping `Bonsai_term.start app` (which
     returns `unit Async.Deferred.Or_error.t` and exits on Ctrl-C by default).
   - `bin/main.ml`: `Command_unix.run Zet.command`.
2. **`dune build` must pass** on the OxCaml switch. This is gate #1.
3. **Add one snapshot test** (`test/dune` + `test/test_zet.ml`). ⚠️ The original
   recipe used `Bonsai_term_test.print_view`/`last_view`, which **do not exist in
   the installed `130.91+190`** (they were added in the newer `references/` clone).
   The idiom that compiles against the installed libs:
   ```ocaml
   open! Core
   open Bonsai_test               (* provides Handle.show *)

   let%expect_test "hello zet renders" =
     let handle = Bonsai_term_test.create_handle Zet.app in
     Handle.show handle;          (* renders the full framed screen *)
     [%expect {| ...ascii... |}]
   ;;
   ```
   `test/dune` libs: `zet bonsai bonsai_term bonsai_term_test bonsai_test core`
   plus `(inline_tests)` and `(pps ppx_jane bonsai.ppx_bonsai)`.
   Run `dune runtest --auto-promote`; the first run exits 1 as it promotes the
   diff, the next run is green. This is gate #2 — the agent-legible feedback loop
   the whole approach depends on.
4. **Run the binary in a real terminal** and confirm it renders + exits cleanly
   on Ctrl-C. (If a real TTY can't be driven from the agent harness, say so
   explicitly rather than claiming success.)
   ⚠️ **Not verified by the agent** — needs an interactive TTY. The exe builds
   and `-help` works (Command wiring OK). Human check:
   `dune exec bin/main.exe` (or run `_build/default/bin/main.exe`), confirm
   "hello zet" centers and Ctrl-C exits cleanly.

**Exit criteria for Phase 0:** `dune build` green, `dune runtest` green with a
real snapshot, binary runs. ✅ build + runtest green; live render is the only
open item (human to confirm).

### Phase 1 — Data layer (sqlite3_utils + FTS5) — DONE ✅ (2026-06-04)

Goal: a typed query module over the existing DB. No UI yet; tested in isolation.

**Pivot from the original plan:** this phase was specced for `caqti-async`, but
caqti was never installed and the Phase-0 wiring already used **synchronous
`sqlite3_utils`** (a high-level typed wrapper over the `sqlite3` bindings, both
present on the switch). We stayed sync rather than taking on caqti's transitive
deps + OxCaml pinning risk for v1. The TUI is single-writer and reads are fast,
so blocking the Async loop on a query is acceptable; bridge to Async at the call
site later if a query ever gets slow. Decision recorded in `decisions.md`.

**What shipped:** `src/db.ml` typed queries — `list_all`, `list_recent ~limit`,
`get_by_id`, `get_by_slug`, `search ~query ?kind ~limit` (FTS5 `MATCH`, ranked),
`tags_of` and `filter_by_tag ~tag` (both via `json_each(metadata,'$.tags')` so
read/filter tag semantics can't diverge). All note-returning queries share one
`note_row` decoder over a fixed 7-column order. SQL is lowercase, river style,
leading commas (user preference). `db/seed.sql` expanded to 4 notes across all
three kinds + an untagged/null-metadata row; `test/test_zet.ml` has expect tests
for every query (ordering, kind filter, FTS ranking, empty-tags, missing-id).
`dune build` + `dune runtest` green.

Original recipe (kept for reference; replace caqti steps with the above):

1. ~~Add deps: `caqti`, `caqti-async`, `caqti-driver-sqlite3`.~~ Superseded —
   used installed `sqlite3` + `sqlite3_utils` instead.
2. `Db` module:
   - Connection open (single-writer; plain open, no WAL gymnastics needed).
   - `Note.t` record mirroring the `notes` row (id, slug, kind, title, body,
     entry_date, metadata-as-`Yojson`/string, timestamps).
   - Queries:
     - `search : query:string -> ?kind -> limit:int -> Note.t list`
       → `SELECT n.* FROM notes_fts f JOIN notes n ON n.id = f.rowid
          WHERE notes_fts MATCH ? ORDER BY rank` (use `bm25()` if we want
          tunable ranking).
     - `list_recent`, `get_by_slug`, `get_by_id`.
     - `tags_of note` → parse `metadata` JSON `$.tags`.
     - `filter_by_tag` → `json_each(metadata, '$.tags')` join (JSON-parsed, not
       indexed; acceptable for v1, revisit if slow).
3. Unit/expect tests against a small fixture DB (build one with the `sqlite3`
   CLI in a test setup, or ship a tiny `.sql` seed).

### Phase 2 — Browse UI (list + detail), read-only — DONE ✅ (2026-06-04)

Goal: the strace_ui-shaped two-pane app, reading real notes.

**What shipped** (`src/zet.ml`): a two-pane list+detail browser. `Model.t` =
`{ cursor; focus }`; `Action.t` reducer (`apply_action_pure ~count`) for cursor
up/down/top/bottom and focus toggle. Panes framed with
`Bonsai_term_border_box` (round corners, focus-colored title + `<tab>` hint),
golden-ratio split (list ~38% / detail ~62%). Keys: Tab toggles focus, C-n/C-p
or arrows move the cursor, C-a/C-e jump top/bottom, Enter focuses detail. The
corpus is loaded **once** via `Db.list_all` in `launch_tui` and passed in as a
plain `~notes` list (not yet a Bonsai-derived value — Phase 3 changes this).
Headless mirrors shipped too: `list` (`-recent N`), `show IDENT`, `search`.

**Known deviations from the original recipe** (carried forward as Phase 3 work):
- `virtual_list` / `scroller` were **not** used — `render_list` is a hand-rolled
  scroll-offset calc, `render_detail` has no scroll at all. Fine for small
  corpora; revisit with the components for large ones / detail scrolling.
- Scroll is hardwired to the list cursor; the detail pane can't scroll even when
  focused (so long notes are unreadable past one screen).
- List pane width derives from the longest *visible* title, so it jitters while
  scrolling; detail pane long lines overflow off-screen (no wrap/truncate).

These three are the subject of the **Phase 2.5 polish pass** below.

#### Phase 2.5 — Browse UI polish (in progress)

Refinement of the shipped Phase 2 UI, before search UX:
1. **Stable list width** — pane width must not depend on visible content;
   derive purely from terminal dimensions so it doesn't jitter on scroll.
2. **Detail text flow** — wrap (or hard-truncate) long body lines to the pane
   width so nothing runs off-screen and unreadable.
3. **Detail scrolling** — when the detail pane has focus, cursor/scroll keys
   scroll the body; long notes become fully readable.

Original recipe (for reference; the shipped form differs as noted above):

1. **Model** (single record, strace_ui style):
   - corpus / current result set (wrap big mutable vecs in the `Box.t`
     phys-equal trick from `virtual_list` — see DECISIONS).
   - `query : string`, `cursor : int`, `focus : List | Detail`,
     `mode : Browse | Search`, `kind_filter`, `tag_filter`.
2. **One `Action.t` variant** + pure `apply_action`. Mirror
   `strace_ui_app.apply_action_pure`.
3. **Components to reuse** (from `bonsai_term_components` / `strace_ui`):
   - `virtual_list` — scrollable, virtualized, filtered list of note titles.
   - `border_box` — pane framing with focus-colored titles.
   - `scroller` — detail pane body scrolling.
4. **Global handler** routes `(mode, focus, key)`: Tab toggles focus,
   arrows/jk move the cursor, Enter opens, `/` enters Search mode. Copy the
   routing skeleton from `strace_ui_app.ml` (the `match model.focus with` block
   around the key handler).
5. Snapshot tests: drive keystrokes (`send_event`), assert the rendered screen.

### Phase 3 — FTS search UX

1. `filter_editor`-style inline input (lift `filter_editor.ml` from strace_ui;
   it already has emacs-ish line editing: kill-to-end, word-motion, etc.).
2. Debounce/recompute the result set incrementally on query change (Bonsai
   incrementality — only re-query when the query string actually changes).
3. Show match ranking; optionally highlight. Snapshot-test the search flow.

### Phase 4 — Metadata editing (tags)

Single-writer, so plain `UPDATE notes SET metadata = ? WHERE id = ?` with a
**partial update that touches only `metadata`, never `body`**. Add/remove tags
via the JSON. FTS triggers keep `notes_fts` in sync automatically (they fire on
`notes` updates). Snapshot-test add/remove tag.

### Phase 5 — Import / export to files

- Export: write selected note body (+ frontmatter from metadata) to a file.
- Import: read a file → upsert into `notes` (writing through `notes` so the FTS
  triggers fire). Scope of "import" (new vs. update, slug collision policy) TBD.

---

## Explicitly deferred (do NOT build until the schema changes)

- **Inter-note links & backlinks.** The current schema has *no* links/edges
  table. Links presumably live as markup in `body` or in `metadata` JSON.
  Following links and backlinks are deferred until we add a `note_links` table
  (a future migration). Don't design the model around links yet.
- **Tag-as-table.** Tags stay in `metadata` JSON for now. A derived
  `note_tags` table for fast tag filtering is a future optimization, not v1.
- **Concurrent-writer safety (WAL, conflict handling).** Single-writer
  assumption holds; revisit only if that changes.
- **Assets / note_assets UI.** Schema supports it; no UI planned for v1.

---

## Known risks / watch-items

- `caqti-driver-sqlite3` building clean on OxCaml — gate it explicitly (Phase 1).
- **No built-in focus management** in bonsai_term (the MLI says so outright). All
  focus + key routing is hand-written. This is the main source of fiddly bugs;
  snapshot tests catch the *visible* cases but not every routing edge.
- **Snapshot blindness to feel/perf.** Tests pin layout + logic, not scroll
  smoothness or render latency. The human (you) is the only oracle for "feels
  good" — same caveat the Jane Street article called out.
- Thin docs; OxCaml-only. Expect to read `references/` source over guides.
