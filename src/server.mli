open Lwt

open Protocol

type connection

type game_state

type message

module CSET : Set.S
module TOPICSET: Set.S
module MSET : Set.S
module QSET : Set.S

(* Hashtable for mapping DESTINATIONS to SUBSCRIBERS *)
module H = Hashtbl

(*
 * [state] is the current state of the server. Stores information including active
 * rooms, subscsribers, game information
 *)
type state

(*
 * [handle_connection conn ()] loops, continuously reading frames from connection
 * input [conn.input] and handles frames accordingly
 *)
val handle_connection : CSET.elt -> unit -> 'a Lwt.t

(*
 * [accept_connection conn] 'accepts' a connection from
 * [conn : descriptor * sockaddr] and creates a channel to the file descriptor,
 * sends a greeting, and calls [handle_connection]
 *
 * Does the initial greeting phase with a new client as well.
 *)
val accept_connection : Lwt_unix.file_descr * Lwt_unix.sockaddr -> unit Lwt.t

(*
 * [create_socket () ] creates a socket of type stream in the internet
 * domain with the default protocol and returns it
 *)
val create_socket : int -> unit -> Lwt_unix.file_descr

(*
 * [create_server] creates a socket with [create_socket] and enters an infinite
 * loop. At each iteration of the loop, it waits for a connection request with
 * accept and treats it with [accept_connection].
 *)
val create_server : int -> unit -> unit -> 'a Lwt.t

(* initialize the server *)
val run_server : int -> bool -> 'a

