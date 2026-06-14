(lang dune 3.17)

(name zet)

(generate_opam_files true)

(license MIT)

(authors "Jones Melton")

(maintainers "Jones Melton <shavah@gmail.com>")

(source
 (github jonesmelton/ztk))

(package
 (name zet)
 (synopsis "A TUI + headless CLI over a personal zettelkasten (SQLite)")
 (description
  "A read-mostly terminal UI over a personal zettelkasten stored in SQLite, with a headless subcommand mirroring every TUI capability. Built on bonsai_term.")
 (depends
  (ocaml
   (>= 5.2.0))
  async
  bonsai
  bonsai_term
  bonsai_term_components
  bonsai_term_test
  core
  core_unix
  ppx_jane
  sqlite3
  sqlite3_utils
  yojson
  zed))
