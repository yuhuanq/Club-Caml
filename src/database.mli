exception DB_exn of string

(* Note: For the purposes of our messagenger, the database is intended to be a
 * server-sided one. *)

(* [initialize db_name] opens a database with the given name
 * in serialized threading and shared cache mode. Returns the corresponding
 * database handler.
 * Note: This is a server-sided database. *)
val initialize: string -> Sqlite3.db

(* [close db] closes database [db] and returns whether it closed or not. *)
val close: Sqlite3.db -> bool

(* [make_table db tb_name tb_layout] creates table with the given name
 * in the given database with the given layout. Raises exception if
 * table is not created successfully. *)
val make_table: Sqlite3.db -> string -> string -> unit

(* [insert db tb_name data] inserts into the given table in the given database
 * the given data and returns a return code. Raises exception if
 * table is not inserted properly. *)
val insert: Sqlite3.db -> string -> string list -> unit

(* [delete_from db tb_name data] deletes data from the given table in the given
 * database and returns a return code. Raises exception if data is not
 * deleted properly. *)
val delete_from: Sqlite3.db -> string -> (string * string) list -> unit

(* [delete_table db tb_name] deletes table from the given database. *)
val delete_table: Sqlite3.db -> string -> unit 
