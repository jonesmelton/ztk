open! Core
module S = Sqlite3_utils

module Note = struct
  (* [slug] and [title] are nullable in the real schema: untitled journal entries have
     neither. [kind] and [body] are always present in practice. *)
  type t =
    { id : int
    ; slug : string option
    ; kind : string
    ; title : string option
    ; body : string
    ; entry_date : string option
    ; metadata : string option
    }
  [@@deriving sexp_of, fields]

  (* A human label for lists/headers. Untitled journal entries fall back to their entry
     date, then to a bare id, so every note shows *something*. *)
  let display_title t =
    match t.title with
    | Some title -> title
    | None ->
      (match t.entry_date with
       | Some date -> date
       | None -> "#" ^ Int.to_string t.id)
  ;;
end

type t = S.t

let open_ path =
  (* Create the containing directory so a default like ~/.zet/zet.db works on a fresh
     machine. Skip for SQLite's special URIs (":memory:", "") which have no real parent. *)
  (match path with
   | ":memory:" | "" -> ()
   | path -> Core_unix.mkdir_p (Filename.dirname path));
  Sqlite3.db_open path
;;

let close t = ignore (Sqlite3.db_close t : bool)

let with_db path ~f =
  let t = open_ path in
  Exn.protect ~f:(fun () -> f t) ~finally:(fun () -> close t)
;;

let exec_script (t : t) sql =
  match Sqlite3.exec t sql with
  | Sqlite3.Rc.OK -> ()
  | rc -> failwithf "exec_script failed: %s" (Sqlite3.Rc.to_string rc) ()
;;

(* The seven note columns, in a fixed order, plus the decoder that maps a row to [Note.t].
   Every note-returning query selects exactly these (in this order) so they can share this
   single typed row description. *)
let note_row =
  S.Ty.(
    ( p6 int (nullable text) text (nullable text) text (nullable text)
      @>> p1 (nullable text)
    , fun id slug kind title body entry_date metadata : Note.t ->
        { id; slug; kind; title; body; entry_date; metadata } ))
;;

let list_all (t : t) : Note.t list =
  S.exec_no_params_exn
    t
    {|   select id
            , slug
            , kind
            , title
            , body
            , entry_date
            , metadata
       from notes
   order by id|}
    ~ty:note_row
    ~f:S.Cursor.to_list
;;

let list_recent (t : t) ~limit : Note.t list =
  let params, row = note_row in
  S.exec_exn
    t
    {|   select id
            , slug
            , kind
            , title
            , body
            , entry_date
            , metadata
       from notes
   order by entry_date desc
            , id desc
      limit ?|}
    ~ty:(S.Ty.p1 S.Ty.int, params, row)
    ~f:S.Cursor.to_list
    limit
;;

let get_by_id (t : t) id : Note.t option =
  let params, row = note_row in
  S.exec_exn
    t
    {|   select id
            , slug
            , kind
            , title
            , body
            , entry_date
            , metadata
       from notes
      where id = ?|}
    ~ty:(S.Ty.p1 S.Ty.int, params, row)
    ~f:S.Cursor.next
    id
;;

let get_by_slug (t : t) slug : Note.t option =
  let params, row = note_row in
  S.exec_exn
    t
    {|   select id
            , slug
            , kind
            , title
            , body
            , entry_date
            , metadata
       from notes
      where slug = ?|}
    ~ty:(S.Ty.p1 S.Ty.text, params, row)
    ~f:S.Cursor.next
    slug
;;

(* FTS5 search over notes_fts (external-content, trigger-synced). [query] is a raw FTS5
   MATCH expression. Results are ranked best-first. When [kind] is given, restrict to that
   note kind. *)
let search (t : t) ~query ?kind ~limit () : Note.t list =
  let params, row = note_row in
  match kind with
  | None ->
    S.exec_exn
      t
      {|   select n.id
              , n.slug
              , n.kind
              , n.title
              , n.body
              , n.entry_date
              , n.metadata
         from notes_fts f
         join notes n
           on n.id = f.rowid
        where notes_fts match ?
     order by rank
        limit ?|}
      ~ty:(S.Ty.(p2 text int), params, row)
      ~f:S.Cursor.to_list
      query
      limit
  | Some kind ->
    S.exec_exn
      t
      {|   select n.id
              , n.slug
              , n.kind
              , n.title
              , n.body
              , n.entry_date
              , n.metadata
         from notes_fts f
         join notes n
           on n.id = f.rowid
        where notes_fts match ?
          and n.kind = ?
     order by rank
        limit ?|}
      ~ty:(S.Ty.(p3 text text int), params, row)
      ~f:S.Cursor.to_list
      query
      kind
      limit
;;

(* Tags of a note, read from its metadata JSON [$.tags] via json_each so the parsing
   matches [filter_by_tag] exactly. Returns [] when the note has no metadata or no tags. *)
let tags_of (t : t) (note : Note.t) : string list =
  S.exec_exn
    t
    {|   select je.value
       from notes n
          , json_each(n.metadata, '$.tags') je
      where n.id = ?
   order by je.id|}
    ~ty:(S.Ty.p1 S.Ty.int, S.Ty.p1 S.Ty.text, Fn.id)
    ~f:S.Cursor.to_list
    note.id
;;

(* Notes carrying [tag] in their metadata [$.tags] array. *)
let filter_by_tag (t : t) ~tag : Note.t list =
  let params, row = note_row in
  S.exec_exn
    t
    {|   select id
            , slug
            , kind
            , title
            , body
            , entry_date
            , metadata
       from notes n
      where exists (
              select 1
                from json_each(n.metadata, '$.tags') je
               where je.value = ?
            )
   order by n.id|}
    ~ty:(S.Ty.p1 S.Ty.text, params, row)
    ~f:S.Cursor.to_list
    tag
;;
