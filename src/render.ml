open! Core
open! Bonsai_term
module Mode = Model.Mode
module Editor = Model.Editor
module Model = Model.Model

let word_runs line =
  let is_word c = Char.is_alphanum c in
  String.to_list line
  |> List.group ~break:(fun a b -> Bool.( <> ) (is_word a) (is_word b))
  |> List.map ~f:(fun chars ->
    let s = String.of_char_list chars in
    ( s
    , match chars with
      | c :: _ -> is_word c
      | [] -> false ))
;;

let highlight ~terms ~base line =
  match terms with
  | [] -> View.text ~attrs:base line
  | _ ->
    let hit = [ Attr.fg Attr.Color.Expert.yellow; Attr.bold ] in
    word_runs line
    |> List.map ~f:(fun (text, is_word) ->
      let matched =
        is_word
        &&
        let lower = String.lowercase text in
        List.exists terms ~f:(fun t -> String.is_prefix lower ~prefix:t)
      in
      View.text ~attrs:(if matched then hit else base) text)
    |> View.hcat
;;

let accent = Attr.Color.Expert.cyan
let dim = Attr.Color.Expert.lightblack

let fit ~width ~height view =
  View.zcat [ view; View.transparent_rectangle ~width ~height ]
;;

let wrap_line ~width line =
  let width = Int.max 1 width in
  if String.length line <= width
  then [ line ]
  else (
    let words = String.split line ~on:' ' in
    let flush rev_lines cur =
      if String.is_empty cur then rev_lines else cur :: rev_lines
    in
    let rec hard_split word acc =
      if String.length word <= width
      then List.rev (word :: acc)
      else hard_split (String.drop_prefix word width) (String.prefix word width :: acc)
    in
    let rev_lines, cur =
      List.fold words ~init:([], "") ~f:(fun (rev_lines, cur) word ->
        if String.length word > width
        then (
          let chunks = hard_split word [] in
          let init = List.drop_last_exn chunks in
          let last = List.last_exn chunks in
          List.fold init ~init:(flush rev_lines cur) ~f:(fun acc c -> c :: acc), last)
        else if String.is_empty cur
        then rev_lines, word
        else if String.length cur + 1 + String.length word <= width
        then rev_lines, cur ^ " " ^ word
        else cur :: rev_lines, word)
    in
    List.rev (flush rev_lines cur))
;;

let detail_body_lines ~width body =
  String.split_lines body |> List.concat_map ~f:(wrap_line ~width)
;;

let render_list_row ~terms ~width ~is_selected (note : Db.Note.t) =
  let marker = if is_selected then "> " else "  " in
  let title = Db.Note.display_title note in
  let label =
    let full = marker ^ title in
    if String.length full > width then String.prefix full (Int.max 0 width) else full
  in
  let base = if is_selected then [ Attr.fg accent; Attr.bold ] else [] in
  highlight ~terms ~base label
;;

let render_list
  ?(empty_label = "(no notes)")
  ?(terms = [])
  ~width
  ~height
  ~cursor
  (notes : Db.Note.t list)
  =
  let content =
    match notes with
    | [] ->
      View.center (View.text ~attrs:[ Attr.fg dim ] empty_label) ~within:{ width; height }
    | _ ->
      let count = List.length notes in
      let scroll_off = Int.max 0 (cursor - (height / 2)) in
      let scroll_off = Int.min scroll_off (Int.max 0 (count - height)) in
      let visible =
        List.sub notes ~pos:scroll_off ~len:(Int.min height (count - scroll_off))
      in
      let rows =
        List.mapi visible ~f:(fun i note ->
          render_list_row ~terms ~width ~is_selected:(scroll_off + i = cursor) note)
      in
      View.vcat rows
  in
  fit ~width ~height content
;;

let detail_header_lines (note : Db.Note.t) =
  1 (* title *)
  + 1 (* "#id  kind" *)
  + (if Option.is_some note.slug then 1 else 0)
  + (if Option.is_some note.entry_date then 1 else 0)
  + 1 (* blank separator *)
;;

let detail_max_scroll ~width ~height (note : Db.Note.t option) =
  match note with
  | None -> 0
  | Some note ->
    let body = detail_body_lines ~width note.body in
    let body_rows = Int.max 0 (height - detail_header_lines note) in
    Int.max 0 (List.length body - body_rows)
;;

let render_detail ?(terms = []) ~width ~height ~scroll (note : Db.Note.t option) =
  let content =
    match note with
    | None -> View.text ~attrs:[ Attr.fg dim ] "(no note selected)"
    | Some note ->
      let meta =
        [ Printf.sprintf "#%d  %s" note.id note.kind ]
        @ (match note.slug with
           | Some s -> [ Printf.sprintf "slug: %s" s ]
           | None -> [])
        @
        match note.entry_date with
        | Some d -> [ Printf.sprintf "date: %s" d ]
        | None -> []
      in
      let header =
        View.vcat
          (highlight
             ~terms
             ~base:[ Attr.fg accent; Attr.bold ]
             (Db.Note.display_title note)
           :: List.map meta ~f:(fun line -> View.text ~attrs:[ Attr.fg dim ] line))
      in
      let all_body = detail_body_lines ~width note.body in
      let body_rows = Int.max 0 (height - detail_header_lines note) in
      let total = List.length all_body in
      let scroll = Int.clamp_exn scroll ~min:0 ~max:(Int.max 0 (total - body_rows)) in
      let visible =
        List.sub
          all_body
          ~pos:scroll
          ~len:(Int.min body_rows (Int.max 0 (total - scroll)))
      in
      View.vcat (header :: View.text "" :: List.map visible ~f:(highlight ~terms ~base:[]))
  in
  fit ~width ~height content
;;

(* The ▏ marker is multibyte; budget arithmetic is byte-based, which is fine because the
   query buffer is pure ASCII. The window slides so the marker is always visible. *)
let list_title ~budget (model : Model.t) =
  match model.mode with
  | Mode.Browse | Help | Edit -> "Notes"
  | Search ->
    let prefix = "Search: " in
    let { Editor.buf; cursor } = model.editor in
    let cursor = Int.clamp_exn cursor ~min:0 ~max:(String.length buf) in
    let avail = Int.max 1 (budget - String.length prefix - 1) in
    let start = Int.max 0 (cursor - avail) in
    let len = Int.min avail (String.length buf - start) in
    let window = String.sub buf ~pos:start ~len in
    let rel = cursor - start in
    let before = String.prefix window rel in
    let after = String.drop_prefix window rel in
    [%string "%{prefix}%{before}▏%{after}"]
;;

let help_sections =
  [ ( "Navigation"
    , [ "C-n / C-p", "next / previous note"
      ; "C-a / C-e", "first / last note"
      ; "Tab", "switch list / detail pane"
      ; "Enter", "focus the detail pane"
      ] )
  ; "Detail pane", [ "C-n / C-p", "scroll body down / up"; "C-a / C-e", "top / bottom" ]
  ; ( "Search"
    , [ "/", "start full-text search"
      ; "C-b / C-f", "move cursor left / right"
      ; "C-k", "kill to end of line"
      ; "C-w", "kill word backward"
      ; "C-d / DEL", "delete char forward / back"
      ; "Enter", "commit query, focus results"
      ; "Esc", "cancel search"
      ] )
  ; ( "Editing"
    , [ "e", "edit selected note's body"
      ; "C-x C-s", "save changes"
      ; "C-g", "cancel without saving"
      ] )
  ; "General", [ "?", "toggle this help"; "C-g / Esc / q", "close help" ]
  ]
;;

let render_help ~width ~height =
  let key_col =
    List.concat_map help_sections ~f:(fun (_, rows) -> List.map rows ~f:fst)
    |> List.map ~f:String.length
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let row (keys, desc) =
    View.hcat
      [ View.text ~attrs:[ Attr.fg accent ] (String.pad_right keys ~len:key_col)
      ; View.text "  "
      ; View.text desc
      ]
  in
  let blocks =
    List.concat_map help_sections ~f:(fun (heading, rows) ->
      (View.text ~attrs:[ Attr.fg dim; Attr.bold ] heading :: List.map rows ~f:row)
      @ [ View.text "" ])
  in
  let blocks =
    match List.rev blocks with
    | _ :: rest -> List.rev rest
    | [] -> []
  in
  let box =
    Bonsai_term_border_box.view
      ~line_type:Round_corners
      ~attrs:[ Attr.fg accent ]
      ~title:"Keybindings"
      ~title_attrs:[ Attr.fg accent; Attr.bold ]
      (View.pad ~l:1 ~r:1 (View.vcat blocks))
  in
  View.center box ~within:{ width; height }
;;
