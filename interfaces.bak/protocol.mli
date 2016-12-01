(* open NetDate *)
open Lwt
open Ptime

(* Types of communications
Format commands (used to change colors)
System commands (quit or change chatrooms)
Game commands (moves in a game, start a game)
Normal messages to the chatroom *)
(* TODO: (* carry data?? *) *)
type comm_type = FormatCmd | SysCmd  | GameCmd |  Message

(* [request] is a record with multiple fields: r_id stores the record ID, comm_type specifies whether it is a simple message or a type of command, r_body is the string that comprises the body and r_timestamp simply stores the timestamp of when the request was sent*)
type request = {
  r_id: int;
  r_type: comm_type;
  r_body: string;
  r_timestamp: time
}

(* [create_socket unit] returns and binds a socket to a local address in the server’s or clients system base and port, depending on which one uses this function. *)
val create_socket :  unit -> Lwt_unix.file_descr

(* [get_type_str] returns the corresponding command type of an inputted string. *)
val get_type_str: string -> comm_type

(* [create_req] returns a request based on the inputted command. *)
val create_req : string -> request

(* Response is the type of responses from the server to the client. [ resp_id ]  corresponds to the  id of the request, [succ] indicates whether the request was executed successfully. Nonzero ints indicate failure. *)
type response = { resp_id : int ; succ : int }

(* [create_response] is used to format a string as a response *)
val create_response: string-> response

