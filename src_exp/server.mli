(* TODO: data structures  *)

(* Address that server listens to *)
val listen_address : Unix.inet_addr
val port : int

val handle_connection : Lwt_io.input_channel -> Lwt_io.output_channel -> unit -> unit Lwt.t

(* [greeting oc] is a message/prompt the server sends to a new connection  *)
val greeting : Lwt_io.output_channel -> unit

(*
 * [accept_connection conn] 'accepts' a connection from
 * [conn : descriptor * sockaddr] and creates a channel to the file descriptor,
 * sends a greeting, and calls [handle_connection]
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
