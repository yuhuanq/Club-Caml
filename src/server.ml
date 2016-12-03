(*
 * server.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *                    Byungchan Lim <bl458@cornell.edu>
 *
 * Distributed under terms of the MIT license.
*)

(* Reminder:
1. Need to finish implementing flushing in server
2. When game ends? *)

(* To keep track of the stuff I'm doing, I am writing this. Will erase
once I am done with games and DB. *)
(* How Game works (Done except step 5)
1. Command starting with #game is written on repl
2. Client makes a game frame containing the command's information and
sends it to server
3. Server accepts the game frame, updates its internal data structure containing
data for all games that are currently being played.
4. Server makes a game_resp frame containing a string representation
of the updated game state and sends it to client
5. Client accepts the game_resp frame and prints the game state on the user's
terminal using the string representation of the game state in game_resp frame.
*)

(* How Database works:
2. Client makes a DATA frame containing the command's information and sends it
to server
3. Server accepts DATA frame. If DATA frame requests less than the number of
units of data that server has stored in its internal data structure, then
*)

open Lwt
open Protocol
open Games

let (>>|) = (>|=)

(* Anonymous bind:
 * syntactic sugar from Lwt.syntax but Merlin doesn't recognize..so manually *)
let (>>) (dt : unit Lwt.t) f = dt >>= (fun () -> f)

(* Game data for a single ongoing game. *)
type game_data = {
  mutable gstate : Games.game_state;
  players : string * string (* This type can change to string list if we
    implement games with more than 2 players. *)
}

type connection = {
  input      : Lwt_io.input_channel;
  output     : Lwt_io.output_channel;
  (* Can only be subscribed to one topic or i.e. be in one chatroom at a time *)
  mutable topic      : string option;
  username   : string
}

type message = {
  id : float; (* The timestamp of the message.
    Unique identifier for messages with the same destination. *)
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

module GDSET = Set.Make(
struct
  type t = game_data
  let compare v1 v2 = Pervasives.compare v1.players v2.players
end)

(* Hashtable for mapping DESTINATIONS to SUBSCRIBERS *)
module H = Hashtbl

type state = {
  mutable connections : CSET.t;
  mutable topics : TOPICSET.t;
  (* mutable queues : QSET.t; *)
  mutable user_map : (string,connection) H.t;
  mutable map : (string,CSET.t) H.t;
  mutable map_msg : (string, MSET.t) H.t;
  mutable map_game_data : (string, GDSET.t) H.t
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
  map_game_data = H.create 20;
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
  state.map_game_data <- H.create 20;
  List.iter (fun elt -> H.add state.map_game_data elt GDSET.empty)
    persist_topics;
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

let handle_send_private frame conn =
  let dest = Protocol.get_header frame "destination" in
  let msg = frame.body in
  let mid = string_of_float (Unix.gettimeofday ()) in
  let recip = List.hd (Str.split private_re dest) in
  let recip_conn = H.find state.user_map recip in
  let message_frame = Protocol.make_message dest mid conn.username msg in
  Protocol.send_frame message_frame recip_conn.output >>
  Lwt_log.info ("sent a private MESSAGE frame to destination: " ^ recip)

(*
 * [handle_send] handles a SEND frame. A SEND commands sends a message to a
 * destination in the system.
*)
(* val handle_send : frame -> connection -> unit Lwt.t  *)
let handle_send frame conn =
  try_lwt
    let topic = Protocol.get_header frame "destination" in
    if Str.string_match topic_re topic 0 then handle_send_topic frame conn
    else if Str.string_match private_re topic 0 then handle_send_private frame
    conn
    else failwith "Invalid send destination"
  with Not_found | _ ->
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
  let _= (Lwt_log.info (conn.username ^ " trying to subscribe to " ^ topic) >>
  return_unit) in
  let conn' = {conn with topic = Some topic} in
  try_lwt
    let conns = H.find state.map topic in
    let conns' = CSET.add conn' conns in
    H.replace state.map topic conns';
    print_endline (conn.username ^ " subscribed to " ^ topic);
    Lwt_log.info (conn.username ^ " subscribed to " ^ topic) >>
    return_unit
  with Not_found ->
    (* if nonexisting topic, create  it and sub conn to it  *)
    state.topics <- TOPICSET.add topic state.topics;
    let conns = CSET.add conn' CSET.empty in
    let msgs = MSET.empty in
    let game_data_set = GDSET.empty in
    H.add state.map topic conns;
    H.add state.map_msg topic msgs;
    H.add state.map_game_data topic game_data_set;
    let _=print_endline ("created new topic: " ^ topic ^ "and " ^ conn.username ^ "
                   subscribed to " ^ topic) in
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
  (* TODO: fix *)
  Lwt_log.info ("Disconnecting user: " ^ conn.username) >>
  Lwt_io.abort conn.output >>= fun _ ->
     (* remove from  connections *)
     state.connections <- CSET.remove conn state.connections;
     H.remove state.user_map conn.username;
     (* remove from subscriptions *)
     let f k v =
       let conns' = CSET.remove conn v in
       H.replace state.map k conns' in
     H.iter f state.map;
     (* terminate the thread now with exn *)
     fail End_of_file

(* [execute_game_cmd game_cmd topic players] updates map_game_data which
 * stores data for all games in each room based on the string [game_cmd] and
 * returns a string representation of the updated game state. *)
let execute_game_cmd game_cmd topic players =
  let game_data_set = H.find state.map_game_data topic in (* When debugging, check here *)
  (* In line 308, there is either 0 or 1 game_data left in game_data_set after
  filtering. *)
  let game_data_set_filtered = GDSET.filter (fun gd ->
    if gd.players = players
      then true
    else false) game_data_set in
  try
  let game_data = GDSET.choose game_data_set_filtered in (* When debugging,
    check length of game_data_set_filtered is 0 or 1. If not, bad code. *)
  let game_cmd = String.lowercase_ascii (String.trim game_cmd) in
  let updated_state = Games.give_updated_game_state game_cmd game_data.gstate in
  Games.game_state_to_string updated_state
  with Not_found ->
    (* Is designed so Not_found only happens when using GDSET.choose. *)
    let game_data = {
      gstate = Games.start_game ();
      players = players
    } in
    let game_data_set' = GDSET.add game_data game_data_set in
    H.replace state.map_game_data topic game_data_set';
    Games.game_state_to_string game_data.gstate

(* [handle_game_server frame conn] makes server handle a GAME frame sent from
 * client. Based on the game command, it updates the internal data structure
 * game_data and if successful, sends a game_resp frame to the
 * destination/chat room.
 * (Similar to how playing chess on FB chat works) *)
let handle_game_server_side frame conn =
  let topic = Protocol.get_header frame "destination" in
  let sender = Protocol.get_header frame "sender" in
  let game_msg = frame.body in
  let idx_of_space = String.index game_msg ' ' in
  let game_opp = String.sub game_msg 0 idx_of_space in
  let players = (sender, game_opp) in
  let game_cmd = String.sub game_msg (idx_of_space+1)
    ((String.length game_msg) - idx_of_space - 1) in
  let updated_game_state_str = execute_game_cmd game_cmd topic players in
  let game_resp_frame = make_game_resp topic updated_game_state_str sender in
  let conns = H.find state.map topic in
  let send_fun connelt =
    ignore_result (Protocol.send_frame game_resp_frame connelt.output) in
  CSET.iter send_fun conns;
  Lwt_log.info ("sent a GAME_RESP frame to destination: " ^ topic) >>
  return_unit

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
  print_endline "in handle frame";
  match frame.cmd with
  | SEND -> let _=print_endline "Received a send frame" in
            handle_send frame conn
  | SUBSCRIBE ->
                 (* print_endline "Received a subscribe frame"; *)
                 Lwt_log.info "Received an subscribe frame" >>= fun _ ->
                 handle_subscribe frame conn
  | UNSUBSCRIBE ->
                  (* let _=print_endline "Received an unsubscribe frame" in *)
                  Lwt_log.info "Received an Unsub farme" >>= fun _ ->
                  handle_unsubscribe frame conn
  | DISCONNECT -> Lwt_log.info "disconnecting a client" >>= fun _ ->
                  handle_disconnect frame conn
  | GAME -> Lwt_log.info "Received a game frame" >>= fun _ ->
            handle_game_server_side frame conn
  | _ -> failwith "invalid client frame"

let handle_connection conn () =
  let rec loop () =
    lwt frame = Protocol.read_frame conn.input in
    handle_frame frame conn >>
    loop ()
  in
    loop ()

(*
 * let rec handle_connection conn () =
 *   Lwt_log.info "in handle connection" >>= fun _ ->
 *   Protocol.read_frame conn.input >>= fun frame ->
 *   handle_frame frame conn >> handle_connection conn ()
 *)

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
let establish_connection ic oc client_id =
  let f fr =
    let _=print_endline "Received some frame" in
    match fr.cmd with
    | CONNECT ->
      print_endline ("New connection from " ^ client_id);
      begin
        (* let reply = make_connected (string_of_int (newi ()) ) in *)
        let reply = {
          cmd = Protocol.CONNECTED;
          headers = ("session",string_of_int (newi ()))::gather_info ();
          body = "";
        } in
        let username = List.assoc "login" fr.headers in
        if H.mem state.user_map username then
          let reply = make_error "" "Username already taken" in
          Protocol.send_frame reply oc >> Lwt_io.abort ic
        else
          Lwt_log.info ("user " ^ username ^ " has logged in.") >>= fun _ ->
          let conn = {input = ic; topic = None; output = oc; username = username} in
          state.connections <- CSET.add conn state.connections;
          H.add state.user_map conn.username conn;
          (* let _ = Protocol.send_frame reply oc in *)
          try_lwt
            Protocol.send_frame reply oc >>=
              fun () ->
                Lwt_log.info ("New connection from " ^
                client_id) >>= fun _ ->
                Lwt.on_failure (handle_connection conn ())
                (fun e -> Lwt_log.ign_error (Printexc.to_string e));
                return_unit
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
  let _=print_endline ("client connected of id "^client_id) in
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
  let _=print_endline "Creating socket\n" in
  let server_socket = create_socket () in
  let rec serve () =
    let client = Lwt_unix.accept server_socket in
    client >>= accept_connection >>= serve
  in serve

(* initialize the server *)
let run_server () =
  let _=print_endline "Running server\n" in
  clean_state ();
  let serve = create_server () in
  Lwt_main.run @@ serve ()

