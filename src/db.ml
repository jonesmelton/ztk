open! Core
module S = Sqlite3_utils

module Note = struct
  type t =
    { id : int
    ; slug : string
    ; kind : string
    ; title : string
    ; body : string
    ; entry_date : string option
    ; metadata : string option
    }
  [@@deriving sexp_of, fields]
end

type t = S.t

let open_ path = Sqlite3.db_open path
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

let list_all (t : t) : Note.t list =
  S.exec_no_params_exn
    t
    "SELECT id, slug, kind, title, body, entry_date, metadata \
     FROM notes ORDER BY id"
    ~ty:
      S.Ty.
        ( p6 int text text text text (nullable text) @>> p1 (nullable text)
        , fun id slug kind title body entry_date metadata : Note.t ->
            { id; slug; kind; title; body; entry_date; metadata } )
    ~f:S.Cursor.to_list
;;
