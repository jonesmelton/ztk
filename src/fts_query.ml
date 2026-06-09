open! Core

(* Sanitizes live keystrokes into a valid FTS5 MATCH expression. A lone quote, trailing
   operator, or bare '-' in raw input would raise an FTS5 syntax error; stripping
   FTS5-special chars and wrapping each token as "tok"* avoids that. An all-empty result
   yields "" which callers treat as no query rather than passing to [Db.search]. *)
let is_special = function
  | '"' | '(' | ')' | '*' | ':' | '-' | '^' -> true
  | _ -> false
;;

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
