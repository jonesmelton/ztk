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

let list_all ?(include_deleted = false) ?kind (t : t) : Note.t list =
  (* A note is soft-deleted when its metadata JSON carries a [$.deleted] timestamp; the
     default corpus hides those. [include_deleted] drops the filter. *)
  let deleted_clause =
    if include_deleted then "" else " and metadata ->> '$.deleted' is null"
  in
  match kind with
  | None ->
    let sql =
      sprintf
        {|   select id
                , slug
                , kind
                , title
                , body
                , entry_date
                , metadata
           from notes
          where 1 = 1%s
       order by id|}
        deleted_clause
    in
    S.exec_no_params_exn t sql ~ty:note_row ~f:S.Cursor.to_list
  | Some kind ->
    let params, row = note_row in
    let sql =
      sprintf
        {|   select id
                , slug
                , kind
                , title
                , body
                , entry_date
                , metadata
           from notes
          where kind = ?%s
       order by id|}
        deleted_clause
    in
    S.exec_exn t sql ~ty:(S.Ty.p1 S.Ty.text, params, row) ~f:S.Cursor.to_list kind
;;

let list_recent ?kind (t : t) ~limit : Note.t list =
  let params, row = note_row in
  match kind with
  | None ->
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
  | Some kind ->
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
        where kind = ?
     order by entry_date desc
              , id desc
        limit ?|}
      ~ty:(S.Ty.(p2 text int), params, row)
      ~f:S.Cursor.to_list
      kind
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

let slug_exists (t : t) slug : bool =
  S.exec_exn
    t
    {|select 1
        from notes
       where slug = ?
       limit 1|}
    ~ty:(S.Ty.p1 S.Ty.text, S.Ty.p1 S.Ty.int, Fn.id)
    ~f:(fun c -> Option.is_some (S.Cursor.next c))
    slug
;;

let unique_slug (t : t) base : string =
  (* Mirror the Ruby [unique_slug]: return [base] if free, else the first [base-N] (N>=2)
     that is. slug is UNIQUE, so a colliding insert would raise; this picks the next gap. *)
  if not (slug_exists t base)
  then base
  else (
    let rec find n =
      let candidate = sprintf "%s-%d" base n in
      if slug_exists t candidate then find (n + 1) else candidate
    in
    find 2)
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

let update_note (t : t) ~id ~slug ~kind ~title ~body ~entry_date ~metadata : unit =
  (* Full-field overwrite mirroring the web [note-edit] POST: every column the form owns
     is rewritten and [updated_at] bumped. slug/entry_date derivation is the caller's
     policy; this just writes what it is handed. The [notes_au] trigger keeps FTS in sync. *)
  S.exec_no_cursor_exn
    t
    {|update notes
         set slug = ?
           , kind = ?
           , title = ?
           , body = ?
           , entry_date = ?
           , metadata = ?
           , updated_at = strftime('%Y-%m-%dT%H:%M:%SZ', 'now')
       where id = ?|}
    ~ty:
      S.Ty.(
        p6 (nullable text) text (nullable text) text (nullable text) (nullable text)
        @>> p1 int)
    slug
    kind
    title
    body
    entry_date
    metadata
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

let append_region (t : t) ~source_id ~source_body ~target_id ~slice : unit =
  with_txn t ~f:(fun t ->
    (* Read the target inside the txn so a missing id aborts the whole thing — otherwise
       the [update_body] no-op would silently trim the source without appending anywhere.
       The slice joins the target's existing body with a blank-line separator. *)
    let target =
      match get_by_id t target_id with
      | Some note -> note
      | None -> failwithf "append_region: no note with id %d" target_id ()
    in
    if source_id = target_id
    then
      (* Appending into the same note we sliced from: the source trim and the append both
         target one row, so do them as a single write off the post-slice remainder.
         Writing them separately would let whichever ran last clobber the other. *)
      update_body t ~id:source_id ~body:(source_body ^ "\n\n" ^ slice)
    else (
      update_body t ~id:target_id ~body:(target.body ^ "\n\n" ^ slice);
      update_body t ~id:source_id ~body:source_body))
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
