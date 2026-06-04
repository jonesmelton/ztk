# zet

A terminal UI for searching, browsing, and editing metadata over a personal
zettelkasten stored in SQLite. Built on
[`bonsai_term`](https://github.com/janestreet/bonsai_term) (OxCaml).

Status: early. Phase 0 (toolchain spike) done; data layer next. See
`docs/plan.md` for build order, `docs/decisions.md` for the why, `CLAUDE.md`
for conventions.

## Develop

Requires the `5.2.0+ox` opam switch.

```
dune build              # compile
dune runtest            # run snapshot tests (silent on success)
dune runtest --force    #   re-run even when nothing changed
dune runtest --auto-promote   # accept new/changed snapshot output
dune exec bin/main.exe  # run the TUI (Ctrl-C to exit)
```
