# zet

A terminal UI for searching, browsing, and editing metadata over a personal
zettelkasten stored in SQLite. Built on
[`bonsai_term`](https://github.com/janestreet/bonsai_term) (OxCaml).

Status: early. Phase 0 (toolchain spike) done; data layer wired (SQLite via
`sqlite3_utils`, reading seeded notes). See `docs/plan.md` for build order,
`docs/decisions.md` for the why, `CLAUDE.md` for conventions.

## Develop

Requires the `5.2.0+ox` opam switch. Common tasks are wrapped in a `justfile`
([`just`](https://github.com/casey/just) — `brew install just`); run `just`
with no args to list them.

```
just build        # compile (dune build)
just test         # run snapshot tests (silent on success)
just test-force   # re-run even when nothing changed
just promote      # accept new/changed snapshot output
just fmt          # format in place
```

Each recipe is a thin wrapper over the equivalent `dune` command, so plain
`dune build` / `dune runtest [--force | --auto-promote]` still work if you'd
rather not use `just`.

## Run

The TUI reads a zettelkasten SQLite file passed via `-db`. To build a fixture DB
from the canonical schema + seed and launch against it:

```
sqlite3 db/zet.db < db/schema.sql   # create schema (one time)
sqlite3 db/zet.db < db/seed.sql     # insert the seed note (one time)
just run                            # launch TUI against db/zet.db (Ctrl-C to exit)
just run db=path/to/other.db        # ...or point at a different db
```

`just run` defaults to `db/zet.db`; `just zet <args>` runs a headless
subcommand (e.g. `just zet search foo`). The underlying command is
`dune exec bin/main.exe -- -db db/zet.db`.

`db/*.db` is gitignored — rebuild it from `db/schema.sql` + `db/seed.sql` any
time. The `sqlite3` CLI on this machine is at
`/opt/homebrew/opt/sqlite/bin/sqlite3`.
