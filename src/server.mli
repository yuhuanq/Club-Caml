open Lwt
open Protocol

(* Address that server listens to *)
val listen_address : Unix.inet_addr

val port : int

type connection = {
  input  : Lwt_io.input_channel;
  output : Lwt_io.output_channel;
  user   : string
}

(* TODO: data structures  *)
(* dictionary/data structure to keep track of Destinations and who's subscribed to which Destination *)
(* data structure: Some kind of Dictinoary with Keys being strings representing the topics/destinations and the values being either
a connection record or connection pair/triple (tuple) *)

val handle_connection : Lwt_io.input_channel -> Lwt_io.output_channel -> unit -> unit Lwt.t

(* [greeting oc] is a message/prompt the server sends to a new connection  *)
val greeting : Lwt_io.output_channel -> unit

val handle_subscribe : frame Lwt.t -> unit Lwt.t
val handle_unsubscribe : frame Lwt.t -> unit Lwt.t
val handle_send : frame Lwt.t -> unit Lwt.t

(*
 * [accept_connection conn] 'accepts' a connection from
 * [conn : descriptor * sockaddr] and creates a channel to the file descriptor,
 * sends a greeting, and calls [handle_connection]
 *
 * Does the initial greeting phase with a new client.
 *)
val accept_connection : Lwt_unix.file_descr * Lwt_unix.sockaddr -> unit Lwt.t

(*
 * [create_socket () ] creates a socket of type stream in the internet
 * domain with the default protocol and returns it
 *)
val create_socket : unit -> Lwt_unix.file_descr

(*
 * [create_server] creates a socket with [create_socket] and enters an infinite
 * loop. At each iteration of the loop, it waits for a connection request with
 * accept and treats it with [accept_connection].
 *)
val create_server : unit -> unit -> 'a Lwt.t
