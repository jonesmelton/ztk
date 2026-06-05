# Content-addressed revision history (design — not yet implemented)

Companion to `plan.md` (build order) and `decisions.md` (rationale). This is an
**agreed design for an eventual schema change**, recorded so it doesn't have to be
re-derived. It is *not* scheduled — current v1 work continues per `plan.md`.

## Context

zet is a read-mostly TUI + headless CLI over a personal zettelkasten in SQLite.
Today `notes` is a mutable row with inline `title`/`body`; there is no history, no
hashing, no migration mechanism (only static `created_at`/`updated_at`). Authoring
happens in an *external* tool; zet imports and lightly edits metadata.

Goal: Fossil-style **content-addressed revision history** — hash the note's content,
keep every version, store text uncompressed (no delta compression: trade disk for
simplicity; corpus is ~279 notes).

### Confirmed decisions
- **Pure content-addressed storage** — a `content` blob table keyed by hash; `notes`
  references the head content by hash; a `note_revisions` table records history.
  `notes` **drops** inline `title`/`body`.
- **Hash covers title + body together** ("the document"). slug/kind/entry_date/metadata
  stay mutable, unversioned columns on `notes`.
- **SHA-256**, lowercase hex.
- **Revisions created on import / external edit** — appended only when imported
  content's hash differs from the current head. zet's own tag/metadata edits create
  **no** revision.

### Verified facts (independently checked, not assumed)
- FTS5 `content=` **accepts a VIEW**. A view-backed external-content table over
  `notes JOIN content` indexes title/body on demand; `'rebuild'` repopulates it —
  tested on sqlite 3.53.1 CLI, including the NULL-title row. Linchpin of the FTS design.
- External-content **triggers are the failure mode**: the `'delete'` command needs the
  exact previously-indexed title/body; a trigger resolving old values via JOIN can
  disagree with the index and corrupt FTS. Avoid triggers; sync imperatively + `'rebuild'`.
- `sha` opam package installs as a **single zero-dep package** on `5.2.0+ox`
  (`digestif` pulls `eqaf`+`digestif`). Neither installed yet — adding one is a
  prerequisite gate (same caution as the caqti decision).
- Real `~/.zet/zet.db`: 279 notes, 213 NULL title+slug, ~275 distinct bodies →
  document-dedup will actually fire. `pragma user_version` = 0.

## FTS decision (the fork)

**External-content FTS pointed at a JOIN view `notes_fts_src` (notes→content), synced
imperatively from the single-writer import path + `'rebuild'` — NO sql triggers.**

Rationale: keeps text in one place (the `content` blob), preserves the existing
`MATCH … JOIN notes` query shape, avoids re-duplicating body on `notes`. The trigger
fragility never arms because writes happen only on import, which already holds the old
head's title/body before moving the pointer. Rejected: contentless FTS (buys nothing —
search already reads text via JOIN, not from FTS) and a denormalized head-cache on
`notes` (re-duplicates body, defeats "pure").

## Schema DDL (lowercase rivers / leading commas)

```sql
create table if not exists content (
  hash        text primary key,    -- lowercase hex sha-256 of canonical bytes
  title       text,                -- nullable
  body        text not null,
  byte_length integer not null,
  created_at  text not null default (datetime('now'))
) without rowid;

create table if not exists note_revisions (
  note_id      integer not null references notes(id),
  seq          integer not null,   -- per-note, 1-based, monotonic
  content_hash text not null references content(hash),
  created_at   text not null default (datetime('now')),
  primary key (note_id, seq)
);
create index if not exists note_revisions_by_hash on note_revisions (content_hash);

create table if not exists notes (
  id           integer primary key,
  slug         text unique,
  kind         text not null check (kind in ('journal','note','inbox')),
  content_hash text not null references content(hash),  -- head pointer
  entry_date   text,
  metadata     text,               -- mutable JSON {"tags":[...]}
  created_at   text not null default (datetime('now')),
  updated_at   text not null default (datetime('now'))
);

create view if not exists notes_fts_src as
    select n.id    as id
         , c.title as title
         , c.body  as body
      from notes n
      join content c
        on c.hash = n.content_hash;

create virtual table if not exists notes_fts using fts5 (
    title
  , body
  , content='notes_fts_src'
  , content_rowid='id'
);
-- NO notes_ai/au/ad triggers. FTS synced by the import path + 'rebuild'.
```

Invariant: `notes.content_hash` == the max-`seq` revision's hash for that note.
`assets` / `note_assets` unchanged.

## Canonical serialization (deterministic, reversible)

Hash input frames title nullability explicitly so `None` ≠ `Some ""`:
- `Some s` → bytes = `"T\n" ^ s ^ "\n" ^ body`
- `None`   → bytes = `"N\n\n" ^ body`

Body appended raw to EOF (no normalization → no spurious churn). A title containing
`\n` makes round-trip ambiguous → `put_content` **raises** (never silently strips).
`byte_length = String.length bytes`; `hash = lowercase hex SHA-256(bytes)`.

OCaml owns this (SQL never reconstructs):
- `Content.canonical_bytes : title:string option -> body:string -> string`
- `Content.hash : title:string option -> body:string -> string`
- `Content.split : canonical:string -> string option * string` (test/verify aid)

## OCaml changes (`src/db.ml`, `src/db.mli`)

- **Dep gate:** add `sha` to `src/dune`; confirm it builds/links on `5.2.0+ox` first.
  Fall back to `digestif` (pure-OCaml backend) if its C stubs misbehave under OxCaml.
- **`Note.t`**: same external shape (still carries `title`/`body`, resolved by JOIN)
  plus `content_hash : string`. Only the `get_by_id` sexp snapshot grows a field.
- **`note_row`**: 8 columns in fixed order
  `id, slug, kind, title, body, entry_date, metadata, content_hash`; `title`/`body`
  come from `c.title`/`c.body`. Shape:
  `p7 int (nullable text) text (nullable text) text (nullable text) (nullable text) @>> p1 text`.
- **All 7 existing queries** gain `join content c on c.hash = n.content_hash` and select
  `c.title, c.body, n.content_hash`. `search` keeps `notes_fts MATCH` (resolves via the
  view). `tags_of` unchanged. `filter_by_tag` adds the content JOIN for projection only.
- **New functions** (`db.mli`):
  - `put_content : t -> title:string option -> body:string -> string` (insert-or-ignore on hash)
  - `module Revision = { note_id; seq; content_hash; title; body; created_at }`
  - `revisions_of : t -> note_id:int -> Revision.t list` (seq asc)
  - `get_revision : t -> note_id:int -> seq:int -> Revision.t option`
  - `import_document : t -> note_id:int -> title:string option -> body:string -> import_result`
    where `import_result = Unchanged | New_revision of { seq; hash }`.
- **`import_document`** (inside `begin immediate … commit`): compute hash; if == head →
  `Unchanged` (no revision, no FTS churn); else fetch OLD head title/body, `insert or
  ignore into content`, `seq = 1 + max(seq)`, insert revision, update `notes.content_hash`
  + `updated_at`, FTS `'delete'` OLD values then rowid-insert, commit.
- `create_note` (new note at seq=1) — spec but defer to the Phase 5 import work.

## Migration

- **Versioning:** `pragma user_version` + a ~20-line OCaml runner (`Db.migrate`): if
  `user_version < N`, run migration N in one transaction, set `user_version := N`.
  Lighter than a framework, safer than a bare one-shot (idempotent, re-run-safe), and
  gives a forward path for the deferred `note_links`/`note_tags` migrations. Expose a
  `zet migrate` subcommand too (dual-interface rule).
- **Migration 1 (backfill, one transaction, single-writer):** create `content` /
  `note_revisions` / `notes_new`; for each legacy row compute the hash **in OCaml**
  (never recompute SHA in SQL), `insert or ignore into content`, insert revision seq=1,
  insert into `notes_new` preserving id/created_at/updated_at; drop old FTS+triggers+notes,
  rename; create view + FTS; `insert into notes_fts(notes_fts) values('rebuild')`;
  `pragma user_version = 1`. Crash leaves user_version=0 → clean retry.
- **One-time manual prerequisite:** back up `~/.zet/zet.db` before first migrate. zet must
  never auto-delete it.
- Pre-commit asserts: `count(notes)==count(note_revisions)`; every `notes.content_hash`
  in `content`; a known `MATCH` returns after rebuild.

## Stays deferred
- Inter-note links/backlinks — still no edges table. (For later: the immutable hashed
  document versions `[[slug]]` markup for free, so a future `note_links` table could be
  derived per revision.)
- Phase 4 tag/metadata editing remains **unversioned** mutable `update notes set
  metadata=?` — creates no revision and (correctly) no FTS churn, since the old
  fire-on-any-update `notes_au` trigger is gone.
- Tag-as-table, concurrent-writer safety, assets UI — unchanged deferral.
- Delta compression + orphan-content GC — out. If GC is ever added it MUST `'rebuild'`
  FTS, never per-row `'delete'`.

## Tests / verification
- Move fixture seeding into an OCaml `seed_demo` helper that calls
  `import_document`/`create_note` (pre-hashed SQL literals would rot). Keep `schema.sql`
  pure DDL.
- New fixture rows: a note byte-identical to an existing one (dedup → shared hash,
  `count(content) < count(notes)`); a note with two revisions (ordering + head==seq-2);
  keep the NULL-title/slug journal (proves `"N\n\n"` framing).
- Expect tests: existing queries still pass (promote `get_by_id` snapshot for new field);
  canonicalization round-trip incl. `None`/`Some ""`/newline-in-body; dedup; `None` vs
  `Some ""` hash differ; revision ordering; re-import identical → `Unchanged`; metadata
  edit → revisions unchanged + body still searchable; FTS correct after migration
  `'rebuild'`; **migration idempotency** (run `Db.migrate` twice → second is a no-op).
- Add `db/seed_legacy.sql` (old inline shape) used only by the migration test; register
  as a `(file …)` dep in `test/dune`.

## Prerequisite gates (do first)
1. `sha` builds + links on `5.2.0+ox` (recommended; fall back `digestif`).
2. Back up `~/.zet/zet.db` before first `migrate`.
3. Confirm view-backed FTS `'rebuild'` works through the `sqlite3` 5.4.1 OCaml binding
   (verified on the CLI; re-confirm in a test, since the binding bundles its own amalgamation).

## Docs to update when implemented
`docs/decisions.md` (content-addressing decision + FTS-via-view/no-triggers rationale +
links-derivable-from-revisions note) and `docs/plan.md` (a new phase for this).
```
