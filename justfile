# list recipes
default:
    @just --list

# build
build:
    dune build

# run the TUI; optionally override the db: just run db=path/to.db
run db="":
    dune exec bin/main.exe -- {{ if db != "" { "-db " + db } else { "" } }}

# run a headless subcommand (e.g. just zet search foo / just zet db=path/to.db search foo)
zet db="" *args:
    dune exec bin/main.exe -- {{ if db != "" { "-db " + db } else { "" } }} {{args}}

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
