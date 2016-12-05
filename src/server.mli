open Lwt
open Protocol

(* Address that server listens to *)
val listen_address : Unix.inet_addr

type connection = {
  input      : Lwt_io.input_channel;
  output     : Lwt_io.output_channel;
  (* Can only be subscribed to one topic or i.e. be in one chatroom at a time *)
  mutable topic      : string option;
  username   : string
}

type game_state = {
  mutable gstate : Games.Tictactoe.t;
  players : string * string;
  mutable turn : string;
}

type message = {
  id : float; (* The timestamp of the message. Unique identifier for messages with the same destination. *)
  conn : connection;
  content : string
}

module CSET : Set.S
module TOPICSET: Set.S
module MSET : Set.S
module QSET : Set.S

(* Hashtable for mapping DESTINATIONS to SUBSCRIBERS *)
module H = Hashtbl

type state = {
  mutable connections : CSET.t;
  mutable topics : TOPICSET.t;
  (* mutable queues : QSET.t; *)
  mutable user_map : (string,connection) H.t;
  mutable map : (string,CSET.t) H.t;
  mutable map_msg : (string, MSET.t) H.t;
  mutable games : (string,game_state) H.t;
  mutable dbase : Database.t
}


(* TODO: data structures  *)
(* dictionary/data structure to keep track of Destinations and who's subscribed to which Destination *)
(* data structure: Some kind of Dictinoary with Keys being strings representing the topics/destinations and the values being either
a connection record or connection pair/triple (tuple) *)

val handle_connection : CSET.elt -> unit -> 'a Lwt.t

(* [greeting oc] is a message/prompt the server sends to a new connection  *)
(* val greeting : Lwt_io.output_channel -> unit *)

val handle_subscribe : Protocol.frame -> CSET.elt -> unit Lwt.t
val handle_unsubscribe : Protocol.frame -> CSET.elt -> unit Lwt.t
val handle_send : Protocol.frame -> CSET.elt -> unit Lwt.t
val handle_disconnect : Protocol.frame -> connection -> unit Lwt.t

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
val create_socket : int -> unit -> Lwt_unix.file_descr

(*
 * [create_server] creates a socket with [create_socket] and enters an infinite
 * loop. At each iteration of the loop, it waits for a connection request with
 * accept and treats it with [accept_connection].
 *)
val create_server : int -> unit -> unit -> 'a Lwt.t


(* initialize the server *)
val run_server : int -> bool -> 'a
