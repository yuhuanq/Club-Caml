open Lwt
open Protocol
open Server

type username=string

type client={ username: username }

val create_request : string -> request

(* [make_req_from_string] takes in the string that the client typed, uses protocol function create_req which makes it a request*)

val make_req_from_string: string -> request

(* [send_req_to_server] sends the request to server and uses server’s function handle_msg to get back the response*)

val send_req_to_server: request-> response

(* After getting a response, we check if it was unsuccessful, and then print a failure message*)

val is_successful : response -> unit

(* [main] is the initialization entry point for client - displays all information and receives input command or messages from user*)
val main: unit -> unit
