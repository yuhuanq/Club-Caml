open Lwt_unix
open Protocol
open Room

(* TODO: data structures  *)

(* Address that server listens to *)
val address_listened : Unix.inet_addr


(* Port that server listens in *)
val port : int


(* Returns a socket based on the address listened and port. *)
val create_socket :  unit -> Lwt_unix.file_descr


(* [connect socket] accepts connections given socket and returns a working
server. If connection is unsuccessful, prints error message in log. *)
val make_server : Lwt_unix.file_descr -> 'a lwt.t

(* [handle_message msg] returns a response based on the msg inputted.
The way type response is defined takes care of invalid inputs.
This function will use functions from Protocol to format the response correctly.
*)
val handle_message: request -> response


(* [main] loops the server and keeps it running. *)
val main: unit -> unit


(* [send_all] sends a message to everyone in given current rom. Uses lwt.io *)
val send_all :  string -> Room.t -> unit
