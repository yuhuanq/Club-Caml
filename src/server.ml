(*
 * server.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
*)

open Lwt
open Protocol

let (>>|) = (>|=)

(* Anonymous bind:
 * syntactic sugar from Lwt.syntax but Merlin doesn't recognize..so manually *)
let (>>) (dt : unit Lwt.t) (f : unit Lwt.t) = dt >>= (fun () -> f)

type connection = {
  input      : Lwt_io.input_channel;
  output     : Lwt_io.output_channel;
  (* Can only be subscribed to one topic or i.e. be in one chatroom at a time *)
  mutable topic      : string option;
  username   : string
}

type message = {
  id : float; (* The timestamp of the message. Unique identifier for messages with the same destination. *)
  conn : connection;
  content : string
}

module CSET = Set.Make(struct
    type t = connection
    let compare v1 v2 = Pervasives.compare v1.username v2.username
  end)

module MSET = Set.Make(
struct
  type t = message
  let compare v1 v2 = Pervasives.compare v1.id v2.id
end)

module USERSET = Set.Make(
struct
  type t = string
  let compare = Pervasives.compare
end)

module TOPICSET = Set.Make(
struct
  type t = string
  let compare = Pervasives.compare
end)

module QSET = Set.Make(
struct
  type t = string
  let compare = Pervasives.compare
end)

(* Hashtable for mapping DESTINATIONS to SUBSCRIBERS *)
module H = Hashtbl

type state = {
  mutable connections : CSET.t;
  mutable topics : TOPICSET.t;
  (* mutable queues : QSET.t; *)
  mutable user_map : (string,connection) H.t;
  mutable map : (string,CSET.t) H.t;
  mutable map_msg : (string, MSET.t) H.t
}

let persist_topics =
  ["/topic/Cornell";
   "/topic/Sports";
   "/topic/News";
   "/topic/Random";
   "/topic/TalkToABot"]

let topic_re = Str.regexp "/topic/"
let private_re = Str.regexp "/private/"

let persist_topics_set = List.fold_left (fun acc elt -> TOPICSET.add elt acc)
                           TOPICSET.empty persist_topics

let state = {
  connections = CSET.empty;
  topics = TOPICSET.empty;
  user_map = H.create 100;
  (* queues = QSET.empty; *)
  map = H.create 10;
  map_msg = H.create 20;
}

(* [clean_state] resets the state to default values *)
let clean_state () =
  state.connections <- CSET.empty;
  state.topics <- persist_topics_set;
  (* state.queues <- QSET.empty; *)
  state.user_map <- H.create 100;
  state.map <- H.create 20;
  List.iter (fun elt -> H.add state.map elt CSET.empty) persist_topics;
  state.map_msg <- H.create 20;
  List.iter (fun elt -> H.add state.map_msg elt MSET.empty) persist_topics;
  ()

(* make server listen on 127.0.0.1:9000 *)
let listen_address = Unix.inet_addr_loopback (* or Sys.argv.(1) *)
let port = 9000 (* or Sys.argv.(2) *)
let backlog = 10 (* max num of connections? not working *)

(* enable logging up to the INFO level *)
let () = Lwt_log.add_rule "*" Lwt_log.Info

(* [get_usernames conns_set] is a string list of the usernames of a connections
 * set*)
let get_usernames conns_set =
  CSET.fold (fun elt acc -> elt.username::acc) conns_set []

(*
 * [get_users_subbed topic] is a string list of the current usernames subbed to a
 * [topic]
*)
let get_users_subbed topic =
  let conns' = H.find state.map topic in get_usernames conns'

(* [gather_info ()] is an assoc list of active topics,num subscsribes*)
let gather_info () =
  H.fold (fun k v acc -> (k,string_of_int (List.length (CSET.elements v)))::acc)
    state.map []


(* [newi] is a unique int *)
let newi =
  let r = ref 0 in
  (fun () -> r:=!r + 1; !r)


let handle_send_topic frame conn =
  failwith "unimplemented"

let handle_send_private frame conn =
  failwith "unimplemented"

(*
 * [handle_send] handles a SEND frame. A SEND commands sends a message to a
 * destination in the system.
*)
(* val handle_subscribe : frame -> connection -> unit Lwt.t  *)
let handle_send frame conn =
  try_lwt
    let topic = Protocol.get_header frame "destination" in
    let msg = frame.body in
    let mid = string_of_float (Unix.gettimeofday ()) in
    let conns = H.find state.map topic in
    let message_frame = Protocol.make_message topic mid conn.username msg in
    let msg_obj = {id = float_of_string mid; conn = conn; content = msg} in
    let msg_objs = H.find state.map_msg topic in
    let msg_objs' = MSET.add msg_obj msg_objs in
    H.replace state.map_msg topic msg_objs';
    let send_fun connelt =
      ignore_result (Protocol.send_frame message_frame connelt.output) in
    CSET.iter send_fun conns;
    Lwt_log.info ("sent a MESSAGE frame to destination: " ^ topic) >>
    return_unit
  with Not_found ->
    let err = make_error "" "Invalid destination header" in
    Protocol.send_frame err conn.output >> return_unit

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
  let conn' = {conn with topic = Some topic} in
  try_lwt
    let conns = H.find state.map topic in
    let conns' = CSET.add conn' conns in
    H.replace state.map topic conns';
    Lwt_log.info (conn.username ^ " subscribed to " ^ topic) >>
    return_unit
  with Not_found ->
    (* if nonexisting topic, create  it and sub conn to it  *)
    state.topics <- TOPICSET.add topic state.topics;
    let conns = CSET.add conn' CSET.empty in
    let msgs = MSET.empty in
    H.add state.map topic conns;
    H.add state.map_msg topic msgs;
    Lwt_log.info ("created new topic: " ^ topic ^ "and " ^ conn.username ^ "
                   subscribed to " ^ topic) >>
    return ()

exception Fail_Unsub

(*
 * [handle_unsubscribe] handles a UNSUBSCRIBE frame. a UNSUBSCRIBE command is
 * used to remove an existing subscribtion - to no longer receive messages from
 * that destination
 *
 * Sends a MESSAGE Frame to all other subscribers to the topic indicating that X
 * user has left the room.
*)
(* val handle_subscribe : frame -> connection -> unit Lwt.t  *)
let handle_unsubscribe frame conn =
  let topic = Protocol.get_header frame "destination" in
  try_lwt
    match conn.topic with
    | Some s when s = topic ->
      conn.topic <- None;
      let conns = H.find state.map topic in
      let conns' = CSET.remove conn conns in
      H.replace state.map topic conns';
      ignore_result (Lwt_log.info ("unsubscribed " ^ conn.username ^ " from " ^
                                   topic));
      let left_message = Protocol.make_message topic "BROKER" (string_of_float
                                                                 (Unix.gettimeofday ())) (conn.username ^ " has left the room.") in
      let send_fun conn = ignore_result (Protocol.send_frame left_message
                                           conn.output) in
      CSET.iter send_fun conns';
      (* Send a INFO frame with info on the curr. active rooms so that client can *)
      (* choose reconnect to a different room *)
      let info_frame = make_info (gather_info ()) in
      Protocol.send_frame info_frame conn.output >>
      if conns' = CSET.empty then
        begin
          H.replace state.map_msg topic MSET.empty;
          begin
            if not (List.mem topic persist_topics) then
              begin state.topics <- TOPICSET.remove topic state.topics;
                return () end
            else return ()
          end
        end
      else return ()
    | _ -> raise Fail_Unsub
  with Not_found ->
    let error = make_error "" "cannot unsubscribe from a nonexsting topic" in
    Protocol.send_frame error conn.output

(* [handle_disconnect] does a graceful disconnect for a client from the server *)
(* val handle_subscribe : frame -> connection -> unit  *)
let handle_disconnect frame conn =
  Lwt_io.abort conn.output >>=
  (fun _ ->
     (* remove from  connections *)
     state.connections <- CSET.remove conn state.connections;
     H.remove state.user_map conn.username;
     (* remove from subscriptions *)
     let f k v =
       let conns' = CSET.remove conn v in
       H.replace state.map k conns' in
     H.iter f state.map;
     Lwt_log.info ("disconnected " ^ conn.username) >>
     return ()
  )

(* [flush_map_message map_message] flushes chat history stored in map_message to
 * the database if the size of the chat history exceeds the limit. It's incomplete
*)
(* let flush_map_message map_message limit =
   let helper topic msgset =
   if List.length (MSET.elements msgset) >= limit
   then (* flush; *) ignore_result (Lwt_log.info ("flushing to disk: " ^ topic))
   else () in
   H.iter helper map_message;
   return () *)

let handle_frame frame conn =
  match frame.cmd with
  | SEND -> handle_send frame conn
  | SUBSCRIBE -> handle_subscribe frame conn
  | UNSUBSCRIBE -> handle_unsubscribe frame conn
  | DISCONNECT -> handle_disconnect frame conn
  | _ -> failwith "invalid client frame"

let rec handle_connection conn () =
  lwt frame = Protocol.read_frame conn.input in
  handle_frame frame conn >> handle_connection conn ()
(* Protocol.read_frame conn.input >>= *)
(* (fun frame -> *)
(* handle_frame frame conn >>= (fun () -> handle_connection conn ())) *)

  (*
 * Lwt_io.read_line_opt conn.input >>=
 * (fun msg ->
 *    match msg with
 *    | Some msg ->
 *      let reply = handle_message msg in
 *      Lwt_io.write_line conn.output reply >>= handle_connection conn
 *    | None -> Lwt_log.info "Connection closed" >>= return)
*)

(* [close_connection conn] closes a connection gracefully *)
let close_connection conn =
  Lwt_io.abort conn.output >>
  begin
    state.connections <- CSET.remove conn state.connections;
    H.remove state.user_map conn.username;
    match conn.topic with
    | Some topic ->
      let conns = H.find state.map topic in
      let conns' = CSET.remove conn conns in
      H.replace state.map topic conns';
      return_unit
    (* TODO: remove from queues as well *)
    | None ->
      return_unit
  end

(*
 * [establish_connection] handles the initial connection phase of the
 * client-server. Try to read a CONNECT frame in, if so => send CONNECTED and
 * return a connection record, else send an ERROR frame
*)
let establish_connection ic oc client_id=
  let f fr =
    match fr.cmd with
    | CONNECT ->
      begin
        (* let reply = make_connected (string_of_int (newi ()) ) in *)
        let reply = {
          cmd = Protocol.CONNECTED;
          headers = ("session",string_of_int (newi ()))::gather_info ();
          body = "";
        } in
        let username = List.assoc "login" fr.headers in
        let conn = {input = ic; topic = None; output = oc; username = username} in
        state.connections <- CSET.add conn state.connections;
        H.add state.user_map conn.username conn;
        (* let _ = Protocol.send_frame reply oc in *)
        try_lwt
          Protocol.send_frame reply oc >>=
          ( fun () -> Lwt.on_failure (handle_connection conn ()) (fun e -> Lwt_log.ign_error
                                                                             (Printexc.to_string e));
            Lwt_log.info ("New connection from " ^ client_id) >>= return )
        with
        | _ ->
          close_connection conn
      end
    | _ ->
      let reply = make_error "" "Expected a CONNECT frame" in
      Protocol.send_frame reply oc >> Lwt_io.abort ic in
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
  establish_connection ic oc client_id

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

