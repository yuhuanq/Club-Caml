(*
 * server.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
*)

open Lwt
open Protocol

type connection = {
  input      : Lwt_io.input_channel;
  output     : Lwt_io.output_channel;
  username   : string
}

module CSET = Set.Make(struct
    type t = connection
    let compare v1 v2 = Pervasives.compare v1.username v2.username
  end)
module USERSET = Set.Make(struct type t = string let compare =
                                                   Pervasives.compare end)
module TOPICSET = Set.Make(struct type t = string let compare =
                                                    Pervasives.compare end)

(* Hashtable for mapping DESTINATIONS to SUBSCRIBERS *)
module H = Hashtbl

type state = {
  mutable connections : CSET.t;
  mutable topics : TOPICSET.t;
  mutable map : (string,CSET.t) H.t;
  (* mutable map_msg : (string,string list) *)
}

let state = {
  connections = CSET.empty;
  topics = TOPICSET.empty;
  map = H.create 10;
}

(* [clean_state] resets the state to default values *)
let clean_state () =
  state.connections <- CSET.empty;
  state.topics <- TOPICSET.empty;
  state.map <- H.create 10;
  ()

(* make server listen on 127.0.0.1:9000 *)
let listen_address = Unix.inet_addr_loopback (* or Sys.argv.(1) *)
let port = 9000 (* or Sys.argv.(2) *)
let backlog = 10 (* max num of connections? not working *)

(* enable logging up to the INFO level *)
let () = Lwt_log.add_rule "*" Lwt_log.Info

(*
 * [handle_send] handles a SEND frame. A SEND commands sends a message to a
 * destination in the system.
*)
(* val handle_subscribe : frame -> connection -> unit Lwt.t  *)
let handle_send frame conn =
  let topic = Protocol.get_header frame "destination" in
  let msg = frame.body in
  let mid = string_of_float (Unix.gettimeofday ()) in
  let conns = H.find state.map topic in
  let message_frame = Protocol.make_message topic mid msg in
  let send_fun conn =
    ignore_result (Protocol.send_frame message_frame conn.output) in
  CSET.iter send_fun conns;
  ignore_result (Lwt_log.info ("sent a MESSAGE frame to destination: " ^ topic));
  return ()

(*
 * [handle_subscribe] handles a SUBSCRIBE frame. a SUBSCRIBE command is used to
 * register to a listen to a given destination
 *
 * if SUBSCRIBE to a nonexisting topic, handle_subscribe will create that topic
 * and subscribe that connection to it
*)
(* val handle_subscribe : frame -> connection -> unit Lwt.t  *)
let handle_subscribe frame conn =
  let topic = Protocol.get_header frame "destination" in
  try
    let conns = H.find state.map topic in
    let conns' = CSET.add conn conns in
    H.replace state.map topic conns';
    ignore_result (Lwt_log.info (conn.username ^ " subscribed to " ^ topic));
    return ()
  with Not_found ->
    (* if nonexisting topic, create  it and sub conn to it  *)
    state.topics <- TOPICSET.add topic state.topics;
    let conns = CSET.add conn CSET.empty in
    H.add state.map topic conns;
    ignore_result (Lwt_log.info ("created new topic: " ^ topic ^ "and " ^
                                 conn.username ^ " subscribed to " ^ topic));
    return ()

(*
 * [handle_unsubscribe] handles a UNSUBSCRIBE frame. a UNSUBSCRIBE command is
 * used to remove an existing subscribtion - to no longer receive messages from
 * that destination
*)
(* val handle_subscribe : frame -> connection -> unit Lwt.t  *)
let handle_unsubscribe frame conn =
  let topic = Protocol.get_header frame "destination" in
  try
    let conns = H.find state.map topic in
    let conns' = CSET.remove conn conns in
    H.replace state.map topic conns';
    ignore_result (Lwt_log.info ("unsubscribed " ^ conn.username ^ " from " ^
    topic));
    return ()
  with Not_found ->
    let error = make_error "" "cannot unsubscribe from a nonexsting topic" in
    Protocol.send_frame error conn.output

(* [handle_disconnect] does a graceful disconnect for a client from the server *)
(* val handle_subscribe : frame -> connection -> unit  *)
let handle_disconnect frame conn =
  let _ = Lwt_io.abort conn.output in
  (* remove from  connections *)
  state.connections <- CSET.remove conn state.connections;
  (* remove from subscriptions *)
  let f k v =
    let conns' = CSET.remove conn v in
    H.replace state.map k conns' in
  H.iter f state.map;
  ignore_result (Lwt_log.info ("disconnected " ^ conn.username));
  return ()

let handle_frame frame conn =
  match frame.cmd with
  | SEND -> handle_send frame conn
  | SUBSCRIBE -> handle_subscribe frame conn
  | UNSUBSCRIBE -> handle_unsubscribe frame conn
  | DISCONNECT -> handle_disconnect frame conn
  | _ -> failwith "invalid client frame"

let handle_message msg = msg

let rec handle_connection conn () =
  Protocol.read_frame conn.input >>=
  (fun frame ->
    handle_frame frame conn >>= (fun () -> handle_connection conn ()))
  (*
   * Lwt_io.read_line_opt conn.input >>=
   * (fun msg ->
   *    match msg with
   *    | Some msg ->
   *      let reply = handle_message msg in
   *      Lwt_io.write_line conn.output reply >>= handle_connection conn
   *    | None -> Lwt_log.info "Connection closed" >>= return)
   *)

(* [newi] is a unique int *)
let newi =
  let r = ref 0 in
  (fun () -> r:=!r + 1; !r)

(*
 * [establish_connection] handles the initial connection phase of the
 * client-server. Try to read a CONNECT frame in, if so => send CONNECTED and
 * return a connection record, else send an ERROR frame
*)
let establish_connection ic oc client_id=
  let f fr =
    match fr.cmd with
    | CONNECT ->
      let reply = make_connected (string_of_int (newi ()) ) in
      let username = List.assoc "login" fr.headers in
      let conn = {input = ic; output = oc; username = username} in
      let _ = Protocol.send_frame reply oc in
      Lwt.on_failure (handle_connection conn ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
      Lwt_log.info ("New connection from " ^ client_id) >>= return
    | _ ->
      let reply = make_error "" "Expected a CONNECT frame" in
      Protocol.send_frame reply oc >>= (fun _ -> Lwt_io.abort ic) in
  Protocol.read_frame ic >>= f

(*
 * [accept_connection conn] 'accepts' a connection from
 * [conn : descriptor * sockaddr] and creates a channel to the file descriptor,
 * sends a greeting, and calls [handle_connection]
*)
let accept_connection (fd, sckaddr) =
  let open Lwt_unix in
  let client_id =
    match sckaddr with
    | ADDR_INET(inet_addr,num) ->
      let open Unix in
      string_of_inet_addr inet_addr
    | _ -> "unknown" in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  (* greeting oc; *)
  establish_connection ic oc client_id
(* Lwt.on_failure (handle_connection ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e)); *)
(* Lwt_log.info ("New connection from " ^ client_id) >>= return *)

(*
 * [create_socket () ] creates a socket of type stream in the internet
 * domain with the default protocol and returns it
*)
let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET(listen_address, port);
  listen sock backlog;
  sock

(*
 * [create_server] creates a socket with [create_socket] and enters an infinite
 * loop. At each iteration of the loop, it waits for a connection request with
 * accept and treats it with [accept_connection].
*)
let create_server () =
  let server_socket = create_socket () in
  let rec serve () =
    let client = Lwt_unix.accept server_socket in
    client >>= accept_connection >>= serve
  in serve

(* initialize the server *)
let () =
  clean_state ();
  let serve = create_server () in
  Lwt_main.run @@ serve ()

