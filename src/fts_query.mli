open! Core

(** Turns arbitrary live keystrokes into a guaranteed-valid FTS5 MATCH expression so a
    half-typed query can never raise a syntax error. Each whitespace-separated token is
    stripped of FTS5-special characters and re-emitted as a quoted prefix phrase; an
    all-empty input yields [""]. *)
val sanitize : string -> string

(** The cleaned, non-empty query words behind {!sanitize}: each whitespace-separated token
    with FTS5-special characters removed, empties dropped. Highlighting reuses these so
    the emphasized words match what was searched. *)
val tokens : string -> string list
