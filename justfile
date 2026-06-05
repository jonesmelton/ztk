db := "db/zet.db"

# list recipes
default:
    @just --list

# build
build:
    dune build

# run the TUI against a db (override: just run db=path/to.db)
run db=db:
    dune exec bin/main.exe

# run a headless subcommand (e.g. just zet search foo)
zet *args:
    dune exec bin/main.exe -- {{args}}

# snapshot tests
test:
    dune runtest

# re-run tests even when nothing changed
test-force:
    dune runtest --force

# accept new/changed snapshot output
promote:
    dune runtest --auto-promote

# format (auto-promote in place)
fmt:
    dune build @fmt --auto-promote
