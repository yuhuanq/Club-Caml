open Ocaml-sqlexpr
open Client


(* inserts messages into the database*)
val insert_msg : username -> string -> unit


(* Returns the history associated with a user of type username*)
val fetch_history : username -> 'a list (* whatever ocaml-sqlexpr  returns/works *)


(*Create a SQLite database with specified string name. Returns true if database successfully   created, false if failed*)
val create_db : string -> boolean


(*Create a table in the current DB with specified string name. Returns true if table successfully    created, false if failed*)
val create_table : string -> boolean


(*Prints all tables in the database with specified string name*)
val show_tables : string -> unit
