open! Core

(* Turns arbitrary live keystrokes into a guaranteed-valid FTS5 MATCH expression, so a
   half-typed query (a lone quote, a trailing operator, a bare '-') can never raise an
   FTS5 syntax error mid-search. Each whitespace-separated token is stripped of
   FTS5-special characters, then re-emitted as a quoted phrase with a trailing [*] for
   prefix matching — so "oc ca" becomes [ "oc"* "ca"* ]. Tokens left empty after stripping
   are dropped; an all-empty input yields [""], which the caller treats as "no query"
   (empty result set) rather than calling [Db.search]. The headless [search] subcommand
   keeps raw FTS5 syntax; this is only for the live TUI box. *)

(* FTS5 treats these as syntax: quotes, parens, the prefix star, column filter, the NOT
   operator, and the NEAR caret. Strip them so a token is always an inert bareword. *)
let is_special = function
  | '"' | '(' | ')' | '*' | ':' | '-' | '^' -> true
  | _ -> false
;;

(* The cleaned, non-empty query words — the exact basis the sanitizer turns into prefix
   phrases. Highlighting reuses these so what's emphasized matches what was searched. *)
let tokens raw =
  raw
  |> String.split_on_chars ~on:[ ' '; '\t'; '\n'; '\r' ]
  |> List.filter_map ~f:(fun token ->
    let cleaned = String.filter token ~f:(fun c -> not (is_special c)) in
    if String.is_empty cleaned then None else Some cleaned)
;;

let sanitize raw =
  tokens raw
  |> List.map ~f:(fun t -> Printf.sprintf "\"%s\"*" t)
  |> String.concat ~sep:" "
;;
