# zet

A terminal UI for searching, browsing, and editing metadata over a personal
zettelkasten stored in SQLite. Built on
[`bonsai_term`](https://github.com/janestreet/bonsai_term) (OxCaml).

Status: early. Phase 0 (toolchain spike) done; data layer wired (SQLite via
`sqlite3_utils`, reading seeded notes). See `docs/plan.md` for build order,
`docs/decisions.md` for the why, `CLAUDE.md` for conventions.

## Develop

Requires the `5.2.0+ox` opam switch.

```
dune build              # compile
dune runtest            # run snapshot tests (silent on success)
dune runtest --force    #   re-run even when nothing changed
dune runtest --auto-promote   # accept new/changed snapshot output
```

## Run

The TUI reads a zettelkasten SQLite file passed via `-db`. To build a fixture DB
from the canonical schema + seed and launch against it:

```
sqlite3 db/zet.db < db/schema.sql        # create schema (one time)
sqlite3 db/zet.db < db/seed.sql          # insert the seed note (one time)
dune exec bin/main.exe -- -db db/zet.db  # run the TUI (Ctrl-C to exit)
```

`db/*.db` is gitignored — rebuild it from `db/schema.sql` + `db/seed.sql` any
time. The `sqlite3` CLI on this machine is at
`/opt/homebrew/opt/sqlite/bin/sqlite3`.
