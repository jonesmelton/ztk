# zet — design decisions & goals

Companion to `plan.md` (the build order) and `CLAUDE.md` (working conventions).
This file records *what* we're building, *why* the stack, and the decisions
made so far so they don't have to be re-litigated.

---

## High-level goal

A TUI for **searching, browsing, and lightly editing metadata** over an existing
personal zettelkasten (a corpus of linked notes) stored in SQLite. **Actual
authoring/editing of note bodies happens in a separate tool** — zet is a
read-mostly browser + metadata editor + import/export front-end, not an editor.

Core v1 capabilities (bounded by the current schema):
- Full-text search over notes (SQLite FTS5).
- Browse results in a list + detail layout.
- Filter by `kind` (journal / note / inbox) and by tag (from `metadata` JSON).
- View a note's body.
- Add/edit metadata (tags) on a note.
- Import/export note bodies to/from files on disk.

---

## The stack: bonsai_term (OxCaml)

Chosen because the user already knows OCaml, has written a TUI in it, and thinks
natively in the Elm / Model-View-Update pattern.

How bonsai_term maps to MVU:

| Elm / MVU | bonsai_term |
|---|---|
| `Model` | `Bonsai.t` state nodes, or (as strace_ui does) one `Model.t` record behind a `Bonsai.Expert.Var` |
| `view : model -> Html` | `View.t` — immutable notty-style image combinators (`vcat`/`hcat`/`zcat`/`pad`/`crop`/`center`) |
| `update : msg -> model -> model` | one **global** `handler : Event.t -> unit Effect.t` that you write by hand |
| runtime/dispatch | `Bonsai_term.start` (Async loop, ~60fps default) |
| `Cmd` / `Sub` | `Effect.t` |

**The single biggest reason for this stack** (given the user is having the agent
write most of it): the **snapshot testing story**. `bonsai_term_test` lets you
`create_handle app`, `send_event`, then `print_view (last_view handle)` into an
`[%expect {| ...ascii screenshot... |}]` block. `dune runtest` diffs it;
`--auto-promote` rewrites it. The test output is legible to a coding agent and
trivial to run — a tight feedback loop where the agent can actually *see* what
it produced.

### Stack strengths
- MVU/Elm knowledge transfers almost directly; no language ramp.
- Snapshot tests make agent-driven development legible and regression-proof.
- Rich reusable component library (`bonsai_term_components`): `tree_view`,
  `scroller`, `textbox`, `text_editor`, `border_box`, `less_keybindings`,
  `click_handler`, `typography`, `catppuccin` theming.
- Real incrementality: large lists re-render cheaply when only the cursor moves.

### Stack weaknesses / risks (eyes open)
- **No built-in focus management.** `bonsai_term.mli` states it outright: there
  is no focused-widget concept, no tab cycling. *We* track focus and route every
  key by hand in the global handler. This is the main source of fiddly bugs.
- **Snapshot blindness to feel/perf.** Tests pin layout + logic, not scroll
  smoothness or render latency. The user is the only oracle for "feels good."
  (The Jane Street article called this out: most human time on strace-ui went to
  exactly the things snapshots can't see.)
- **Thin docs, OxCaml-only, Jane-Street-shaped.** The bonsai_web guide is the
  closest manual and half of it is vdom-specific. We read `references/` source.
- **Async-based.** Any blocking data source must be bridged into Async.
- **Lock-in** to the OxCaml ecosystem.

### Reference template
`references/strace_ui/` is the structural model: a two-pane (list left, detail
right) bonsai_term app with an inline filter editor and manual `Tab`-based focus
routing. Its shape:
- `Model.t` record + one big `Action.t` variant + pure `apply_action_pure`.
- `Focus.t = Syscall_list | Detail_pane`; the key handler does
  `match model.focus with ...` to route. We copy this skeleton.
- `virtual_list.ml` — virtualized/filtered/scrollable list (for big corpora).
- `filter_editor.ml` — inline text input with emacs-ish line editing.

---

## Data layer decisions

- **SQLite via synchronous `sqlite3_utils`** (a typed, high-level wrapper over
  the `sqlite3` bindings; both already installed on the switch). *Originally*
  specced as `caqti` + `caqti-async` + `caqti-driver-sqlite3`, but caqti was
  never installed and pulls a large transitive-dep tree with OxCaml pinning
  risk. We pivoted to sync (2026-06-04). Rationale: single-writer, read-mostly,
  small corpus — queries are fast and blocking the Async render loop briefly is
  acceptable for v1. `sqlite3_utils`' `Ty` combinators give the same typed-query
  ergonomics caqti would have. If a query ever gets slow, bridge it to Async at
  the call site (`In_thread.run`) rather than rewriting the data layer. Queries
  are written lowercase, river-aligned, leading commas (`src/db.ml`).
- **Single-writer assumption.** The TUI and any external editor are never writing
  the DB concurrently. So: no WAL-mode gymnastics, no conflict handling. Plain
  connection open. Metadata edits use partial `UPDATE ... SET metadata = ?`
  (never clobber `body`). Revisit only if the single-writer assumption breaks.

### Schema (current — authoritative)

```sql
notes(id, slug UNIQUE, kind CHECK in ('journal','note','inbox'),
      title, body, entry_date, metadata /* JSON, e.g. {"tags":[...]} */,
      created_at, updated_at)
assets(id, filename UNIQUE, mime_type, size_bytes, note_id→notes, created_at)
note_assets(note_id→notes, asset_id→assets, PK(note_id,asset_id))
notes_fts  -- FTS5, content='notes', content_rowid='id', columns (title, body)
-- triggers notes_ai / notes_au / notes_ad keep notes_fts in sync on
-- insert/update/delete of notes.
```

Key facts derived from the schema:
- **`slug` and `title` are NULLABLE.** In the real corpus ~76% of notes (the
  untitled journal entries) have NULL `slug` *and* NULL `title`; `kind`/`body`
  are always present. So `Note.t` models `slug : string option` and
  `title : string option`. `Note.display_title` gives a non-empty label (title →
  entry_date → `#id`) for lists/headers. **Incident (2026-06-04):** the original
  `note_row` decoder + fixture schema wrongly assumed these were NOT NULL, so
  bare `zet` crashed with `Sqlite3_utils.Type_error` on the first real journal
  row. Fixed; fixture schema (`db/schema.sql`) relaxed to match reality and the
  seed now includes a NULL-slug/title row so tests cover it.
- **FTS is external-content + trigger-synced.** Search =
  `SELECT n.* FROM notes_fts f JOIN notes n ON n.id = f.rowid
   WHERE notes_fts MATCH ? ORDER BY rank`. Because writes go through `notes`,
  the triggers keep FTS current with no work from us.
- **Tags live in `metadata` JSON** (`$.tags`), not a table. Tag filtering uses
  `json_each` (parsed per query, not indexed) — fine for v1.
- **There is NO links/edges table.** Inter-note links are not first-class in the
  schema. See deferred items.

---

## CLI surface: every TUI feature has a headless subcommand mirror

zet is a **dual interface** — the TUI *and* a complete headless CLI. The rule
(user's call, 2026-06-04): any capability exposed in the TUI must also have a
scriptable subcommand, so the whole tool is usable without the terminal UI.

- Top-level `zet` is a `Command.group`. Bare `zet` (no subcommand) launches the
  TUI on the default db via the group's `?body` (sync — it boots the Async
  scheduler with `Thread_safe.block_on_async`). `zet tui [-db PATH]` is the
  explicit, flag-taking form.
- **Core's `Command` can't make one command both take flags and host
  subcommands** — that's why bare `zet` is default-db-only and `-db` requires
  the explicit `tui` subcommand. Don't try to "fix" this; it's a Command limit.
- Headless mirrors are plain `Command.basic` commands that call the same `Db`
  functions and print results via `Zet.print_notes` (stable
  `id<TAB>slug<TAB>kind<TAB>title`, one per line — greppable). First one shipped:
  `zet search QUERY [-kind ...] [-limit ...]`.
- **When adding a feature, add its subcommand as a peer in the group** (and an
  expect test for the output format). As this grows, extract the headless
  commands into a `Cli` module; in `zet.ml` for now while small.

---

## Keyboard conventions

Emacs-style bindings throughout — both TUI and any textbox/input widgets.

**Navigation (list, panes):**
- `C-n` / `C-p` — next / previous item (down / up)
- `C-f` / `C-b` — scroll forward / back (right / left, or page-forward where applicable)
- `C-v` — page down; `M-v` — page up
- `C-a` / `C-e` — beginning / end of list (jump-to-top / jump-to-bottom)
- `C-g` — cancel / dismiss / close overlay; escape hatch from any mode

**Line editing (search box, metadata input):**
- `C-a` / `C-e` — beginning / end of line
- `C-k` — kill to end of line
- `C-w` — kill word backward
- `C-d` — delete char forward; `DEL` — delete char backward
- `C-f` / `C-b` — forward / backward char

**Focus switching:** `TAB` moves between panes (as in strace_ui skeleton).

**Help:** `?` (in Browse) toggles a centered keybinding cheat-sheet overlay;
`?` / `C-g` / `Esc` / `q` dismiss it. The overlay's binding list is built from
`help_sections` in `src/zet.ml` — keep it in sync with this section.

**Rationale:** strace_ui's `filter_editor.ml` already implements emacs line-editing.
The list navigation keys match what Emacs users expect from `dired`/`helm`/`ivy`.
`C-g` is the universal bail-out — it should never be consumed by a focused widget
when the user means "get me out of here."

---

## Scope decisions

**In scope for v1:** FTS search, list+detail browse, kind filter, tag filter
(JSON), view body, add/edit tags, import/export bodies to files.

**Explicitly deferred until the schema changes** (user's call: "any feature not
supported by the current schema we will defer; we can change the schema later"):
- **Inter-note links & backlinks** — no edges table exists. Following links /
  backlinks wait for a future `note_links` migration. Do not design the model
  around links yet.
- **Tag-as-table** — a derived `note_tags` table for fast filtering is a future
  optimization, not v1.
- **Concurrent-writer safety** (WAL, conflict resolution) — single-writer holds.
- **Assets / note_assets UI** — schema supports it; no UI planned for v1.

---

## Open questions (not yet decided)

- Where links *actually* live today (`[[slug]]` in body vs. a `metadata` key) —
  deferred, but we'll need the answer before the links feature.
- Import semantics: new-vs-update, slug-collision policy.
- Ranking: raw FTS `rank` vs. tuned `bm25()` weights (title > body?).
