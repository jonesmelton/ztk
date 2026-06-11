open! Core
module S = Sqlite3_utils

module Note = struct
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
  (* mkdir_p so ~/.zet/zet.db works on a fresh machine; skip for ":memory:"/""  *)
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

let with_txn (t : t) ~f =
  exec_script t "begin";
  match f t with
  | result ->
    exec_script t "commit";
    result
  | exception exn ->
    (* Roll back on any failure so a partial multi-statement write never commits. The
       rollback itself is best-effort: if it fails the original [exn] is still the
       meaningful error, so swallow the rollback's. *)
    (try exec_script t "rollback" with
     | _ -> ());
    raise exn
;;

let note_row =
  S.Ty.(
    ( p6 int (nullable text) text (nullable text) text (nullable text)
      @>> p1 (nullable text)
    , fun id slug kind title body entry_date metadata : Note.t ->
        { id; slug; kind; title; body; entry_date; metadata } ))
;;

let list_all ?(include_deleted = false) (t : t) : Note.t list =
  (* A note is soft-deleted when its metadata JSON carries a [$.deleted] timestamp; the
     default corpus hides those. [include_deleted] drops the filter for the headless
     [list --all] and the sweep path. *)
  let sql =
    if include_deleted
    then
      {|   select id
              , slug
              , kind
              , title
              , body
              , entry_date
              , metadata
         from notes
     order by id|}
    else
      {|   select id
              , slug
              , kind
              , title
              , body
              , entry_date
              , metadata
         from notes
        where metadata ->> '$.deleted' is null
     order by id|}
  in
  S.exec_no_params_exn t sql ~ty:note_row ~f:S.Cursor.to_list
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

let update_body (t : t) ~id ~body : unit =
  S.exec_no_cursor_exn
    t
    {|update notes
         set body = ?
       where id = ?|}
    ~ty:S.Ty.(p2 text int)
    body
    id
;;

let set_deleted (t : t) ~id ~deleted : unit =
  (* Soft-delete marker lives in the metadata JSON at [$.deleted]. Marking writes an
     RFC3339-ish [datetime('now')] timestamp (so a later sweep can filter by age);
     unmarking removes the key. [coalesce(metadata, '{}')] handles notes with NULL
     metadata — [json_set(NULL, ...)] would otherwise yield NULL and silently no-op. The
     [notes_au] trigger keeps FTS in sync on the update. *)
  let sql =
    if deleted
    then
      {|update notes
           set metadata = json_set(coalesce(metadata, '{}'), '$.deleted', datetime('now'))
         where id = ?|}
    else
      {|update notes
           set metadata = json_remove(metadata, '$.deleted')
         where id = ?|}
  in
  S.exec_no_cursor_exn t sql ~ty:S.Ty.(p1 int) id
;;

let sweep_deleted (t : t) : int =
  (* Hard-delete every soft-deleted note in one transaction and return the count removed.
     Irreversible: this is the only place rows actually leave the table. The [notes_ad]
     trigger drops their FTS rows. Counted before the delete so the number reflects what
     was swept. *)
  with_txn t ~f:(fun t ->
    let n =
      S.exec_no_params_exn
        t
        {|select count(*)
            from notes
           where metadata ->> '$.deleted' is not null|}
        ~ty:S.Ty.(p1 int, Fn.id)
        ~f:(fun c -> S.Cursor.next c |> Option.value ~default:0)
    in
    S.exec0_exn
      t
      {|delete from notes
         where metadata ->> '$.deleted' is not null|};
    n)
;;

let create_note (t : t) ~slug ~kind ~title ~body ~entry_date ~metadata : int =
  S.exec_no_cursor_exn
    t
    {|insert into notes
            ( slug
            , kind
            , title
            , body
            , entry_date
            , metadata )
       values (?, ?, ?, ?, ?, ?)|}
    ~ty:
      S.Ty.(p6 (nullable text) text (nullable text) text (nullable text) (nullable text))
    slug
    kind
    title
    body
    entry_date
    metadata;
  Int64.to_int_exn (Sqlite3.last_insert_rowid t)
;;

let extract_region
  (t : t)
  ~source_id
  ~source_body
  ~new_slug
  ~new_kind
  ~new_title
  ~new_body
  ~new_entry_date
  ~new_metadata
  : int
  =
  with_txn t ~f:(fun t ->
    let new_id =
      create_note
        t
        ~slug:new_slug
        ~kind:new_kind
        ~title:new_title
        ~body:new_body
        ~entry_date:new_entry_date
        ~metadata:new_metadata
    in
    update_body t ~id:source_id ~body:source_body;
    new_id)
;;

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
