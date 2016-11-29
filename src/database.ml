open Sqlite3

exception DB_exn of string

(* [initialize db_name] opens a database with the given name
 * in serialized threading and shared cache mode. Returns the corresponding
 * database handler. *)
let initialize db_name =
  db_open ~mutex:`FULL ~cache:`SHARED db_name

(* [close db] closes database [db] and returns whether it closed or not. *)
let close db_name =
  db_close db_name

(* [make_table db tb_name tb_layout] creates table with the given name
 * in the given database with the given layout. Raises exception if
 * table is not created successfully. *)
let make_table db tb_name tb_layout =
  let statement = "CREATE TABLE " ^ tb_name ^ "(" ^ tb_layout ^ ");" in
  match exec db statement with
  | Sqlite3.Rc.OK -> ()
  | _ -> raise (DB_exn(errmsg db))

(* [insert db tb_name data] inserts into the given table in the given database
 * the given data and returns a return code. Raises exception if
 * table is not inserted properly. *)
let insert db tb_name data =
  let statement = "INSERT INTO " ^ tb_name ^ " VALUES(" in
  let rec make_insert_stmt stmt lst =
    match lst with
    | [] -> stmt
    | h::t ->
      if t = []
        then make_insert_stmt (stmt ^ h ^ ");") t
      else
        make_insert_stmt (stmt ^ h ^ ", ") t in
  let statement = make_insert_stmt statement data in
  match exec db statement with
  | Sqlite3.Rc.OK -> ()
  | _ -> raise (DB_exn(errmsg db))

(* [delete db tb_name data] deletes data from the given table in the given database
 * and returns a return code. Raises exception if
 * table is not deleted properly. *)
let delete db tb_name data =
  let statement = "DELETE FROM " ^ tb_name ^ "WHERE " in
  let rec make_delete_stmt stmt lst =
    match lst with
    | [] -> stmt
    | (k,v)::t ->
      if t = []
        then make_delete_stmt (stmt ^ k ^ "=" ^ v ^ ");") t
      else
        make_delete_stmt (stmt ^ k ^ "=" ^ v ^ " AND ") t in
  let statement = make_delete_stmt statement data in
  match exec db statement with
  | Sqlite3.Rc.OK -> ()
  | _ -> raise (DB_exn(errmsg db))
