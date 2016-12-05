(*
 * server.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *                    Byungchan Lim <bl458@cornell.edu>
 *
 * Distributed under terms of the MIT license.
*)

(* Reminder:
1. Need to finish implementing flushing in server *)

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

let (>>|) = (>|=)


(* let flog = Lwt_main.run (Lwt_log.file "server.log" ()) *)

(* Anonymous bind:
 * syntactic sugar from Lwt.syntax but Merlin doesn't recognize..so manually *)
let (>>) (dt : unit Lwt.t) f = dt >>= (fun () -> f)

type game_state = {
  mutable gstate : Games.Tictactoe.t;
  players : string * string;
  mutable turn : string;
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

module H = Hashtbl

type state = {
  mutable connections : CSET.t;
  mutable topics : TOPICSET.t;
  (* mutable queues : QSET.t; *)
  mutable user_map : (string,connection) H.t;
  mutable map : (string,CSET.t) H.t;
  mutable map_msg : (string, MSET.t) H.t;
  mutable games : (string,game_state) H.t;
}

let persist_topics =
  ["/topic/Cornell";
   "/topic/Sports";
   "/topic/News";
   "/topic/Random";
   "/topic/Chatbot"]

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
  games = H.create 20;
}

(* [clean_state] resets the state to default values *)
let clean_state () =
  state.connections <- CSET.empty;
  state.topics <- persist_topics_set;
  (* state.queues <- QSET.empty; *)
  H.reset state.user_map;
  H.reset state.map;
  List.iter (fun elt -> H.add state.map elt CSET.empty) persist_topics;
  H.reset state.map_msg;
  List.iter (fun elt -> H.add state.map_msg elt MSET.empty) persist_topics;
  H.reset state.games

(* make server listen on 127.0.0.1:9000 *)
let listen_address = Unix.inet_addr_loopback (* or Sys.argv.(1) *)
let backlog = 100 (* max num of connections? not working *)


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

(* [gather_info ()] is an assoc list of active topics, subscsribes*)
let gather_info () =
  let get_usernames_str clst =
    CSET.fold (fun elt acc ->
      if String.length acc = 0 then elt.username
      else elt.username ^ "," ^ acc) clst "" in
  H.fold (fun k v acc -> (k,get_usernames_str v)::acc)
  state.map []

let room_subs topic =
  let conns = H.find state.map topic in
  let subs = CSET.fold (fun elt acc ->
    if String.length acc = 0 then elt.username
    else elt.username ^ "," ^ acc) conns "" in
  [topic,subs]
  (*
   * with
   *   Not_found ->
   *     let err = Protocol.make_error "Invalid room" "No stats for this room." in
   *)


(* [get_room_stats () ] is an assoc list of active topics, num subscribers *)
let room_nums () =
  H.fold (fun k v acc -> (k,string_of_int (List.length (CSET.elements
  v)))::acc) state.map []

(* [newi] is a unique int *)
let newi =
  let r = ref 0 in
  (fun () -> r:=!r + 1; !r)

(* [send_all topic frame] sends the frame to all connections in conns *)
let send_all (conns : CSET.t) frame =
  Lwt_list.iter_p (fun conn -> Protocol.send_frame frame conn.output)
  (CSET.elements conns)

let current_time ()=
  let open Unix in
  let unixtime=Unix.localtime (Unix.gettimeofday ()) in
  let hr=string_of_int unixtime.tm_hour in
  let min=string_of_int unixtime.tm_min in
  let sec=string_of_int unixtime.tm_sec in
  hr^min^sec


(* temp: initiate chatbot once on server_start: so now every single msg to it
 * will be continuous *)
let () = Chatbot.init ()
let handle_chatbot frame conn =
  (* TODO: continue conversation, rn before every msg, init is called *)
  (* Chatbot.init (); *)
  let topic = Protocol.get_header frame "destination"  in
  let botre = Chatbot.ask frame.body in
  let echoMsg = Protocol.make_message topic (current_time ())
    conn.username frame.body in
  Lwt_log.info ("Bot response is: " ^ botre) >>
  let reply = Protocol.make_message topic
  (current_time ()) "Artificial Conversational Entity" botre in
  Lwt_log.info "Right before let conns = H.find state.map topic L185" >>
  let conns = H.find state.map topic in
  Lwt_log.info "Right before Lwt_list.iter_p L187" >>
  send_all conns echoMsg >>
  send_all conns reply

let handle_send_topic frame conn =
  let topic = Protocol.get_header frame "destination" in
  if topic = "/topic/Chatbot" then handle_chatbot frame conn else
    let msg = frame.body in
    let mid = current_time () in
    let conns = H.find state.map topic in
    let message_frame = Protocol.make_message topic mid conn.username msg in
    let msg_obj = {id = float_of_string mid; conn = conn; content = msg} in
    let msg_objs = H.find state.map_msg topic in
    let msg_objs' = MSET.add msg_obj msg_objs in
    H.replace state.map_msg topic msg_objs';
    (*
     * let send_fun connelt =
     *   ignore_result (Protocol.send_frame message_frame connelt.output) in
     * CSET.iter send_fun conns;
     *)
    send_all conns message_frame >>
    Lwt_log.info ("sent a MESSAGE frame to destination: " ^ topic) >>
    return_unit

let handle_send_private frame conn =
  let dest = Protocol.get_header frame "destination" in
  let msg = frame.body in
  let mid = current_time () in
  let recip = List.hd (Str.split private_re dest) in
  let recip_conn = H.find state.user_map recip in
  let message_frame = Protocol.make_message dest mid conn.username msg in
  Lwt_list.iter_p (fun conn -> Protocol.send_frame message_frame conn.output)
  [conn;recip_conn]
  (*
   * Protocol.send_frame message_frame recip_conn.output >>
   * Lwt_log.info ("sent a private MESSAGE frame to destination: " ^ recip)
   *)

(*
 * [handle_send] handles a SEND frame. A SEND commands sends a message to a
 * destination in the system.
*)
(* val handle_send : frame -> connection -> unit Lwt.t  *)
let handle_send frame conn =
  try_lwt
    let topic = Protocol.get_header frame "destination" in
    if Str.string_match topic_re topic 0 then handle_send_topic frame conn
    else if Str.string_match private_re topic 0 then
    lwt ()=Lwt_log.info "Did receive a request for a private message" in
    handle_send_private frame conn
    else failwith "Invalid send destination"
  with
    Not_found ->
      let err = make_error "" "Invalid destination header" in
      Protocol.send_frame err conn.output >> return_unit
    | _ ->
      let err = make_error "" "Uh oh. Something Went Wrong!" in
      Protocol.send_frame err conn.output >> return_unit

let option_to_str s=
  match s with
  |Some x-> x
  |None -> "None"


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
  lwt () = Lwt_log.info (conn.username ^ " trying to subscribe to " ^ topic) in
  conn.topic<-Some topic;
  let conn' = {conn with topic = Some topic} in
  try_lwt
    let conns = H.find state.map topic in
    let conns' = CSET.add conn' conns in
    H.replace state.map topic conns';
    Lwt_log.info (conn.username ^ " subscribed to " ^ topic) >>
    let message = Protocol.make_message topic (current_time ()) "SERVER"
      (conn.username ^ " has joined the room.") in
    let stats = Protocol.make_stats "room_inhabitants" (room_subs topic) in
    lwt () = Lwt_log.info ("Current connection topic " ^ (option_to_str conn.topic)) in
    send_all conns' message >>
    send_all conns' stats
  with Not_found ->
    (* if nonexisting topic, create  it and sub conn to it  *)
    (* TODO: transfer this check and the username len check to client *)
    if String.length topic > 50 || String.length topic < 1 then
      let reply = make_error "Invalid room name" "Room names must be between 1
      and 50 characters." in
      Protocol.send_frame reply conn.output
    else
      begin
        state.topics <- TOPICSET.add topic state.topics;
        let conns = CSET.add conn' CSET.empty in
        let msgs = MSET.empty in
        H.add state.map topic conns;
        H.add state.map_msg topic msgs;
        Lwt_log.info ("created new topic: " ^ topic ^ "and " ^ conn.username ^ "
                       subscribed to " ^ topic) >>
        let message = Protocol.make_message topic (current_time ()) "SERVER"
          (conn.username ^ " has joined the room.") in
        let stats = Protocol.make_stats "room_inhabitants" (room_subs topic) in
        send_all conns message >>
        send_all conns stats
      end

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
  Lwt_log.info ("topic being unsubbed from "^topic)>>
  Lwt_log.info ("conn's topic is "^ (option_to_str conn.topic))>>
  try_lwt
    match conn.topic with
    | Some s when s = topic ->
      let conns = H.find state.map topic in
      let conns' = CSET.remove conn conns in
      H.replace state.map topic conns';
      ignore_result (Lwt_log.info ("unsubscribed " ^ conn.username ^ " from " ^
                                   topic));
      conn.topic <- None;
      let left_message = Protocol.make_message topic "BROKER" (current_time ())
        (conn.username ^ " has left the room.") in
      (*
       * let send_fun conn = ignore_result (Protocol.send_frame left_message
       *                                      conn.output) in
       * CSET.iter send_fun conns';
       *)
      send_all conns' left_message >>
      (* TODO: *)
      let stat_frame = Protocol.make_stats "room_inhabitants" (room_subs topic) in
      let stat_frame_num = Protocol.make_stats "num_in_rooms" (room_nums ()) in
      Protocol.send_frame stat_frame_num conn.output >>
      (* update eveyrone elses userlist *)
      send_all conns' stat_frame >>
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

exception Disconnected_EOF

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
     fail Disconnected_EOF

let get_player' p (p1,p2) =
  if p = p1 then p2
  else if p = p2 then p1
  else raise Not_found

let send_turn_error frame conn =
  let fr = Protocol.make_error "game" "It's not your turn!" in
  Protocol.send_frame fr conn.output

let handle_game frame conn =
  Lwt_log.info "in handle_game frame" >>
  try_lwt
    let dest = Protocol.get_header frame "destination" in
    let chal = String.trim (Protocol.get_header frame "challenge") in
    if chal = "true" then
      (* replace any existing games *)
      let dest = Protocol.get_header frame "destination" in
      let opp = Protocol.get_header frame "opponent" in
      let gstate = Games.Tictactoe.new_game () in
      let newg = { gstate = gstate; players = (conn.username,opp); turn =
        conn.username} in
      H.add state.games conn.username newg;
      H.add state.games opp newg;
      let str_rep = Games.Tictactoe.to_string gstate in
      let reply = Protocol.make_game_message str_rep newg.players Games.Tictactoe.instructions in
      let conns = H.find state.map dest in
      send_all conns reply
    else if chal="false" then
      let dest = Protocol.get_header frame "destination" in
      let st = H.find state.games conn.username in
      Lwt_log.info ("found existing game for user: " ^ conn.username) >>
      if st.turn <> conn.username then
        Lwt_log.info ("st.turn is:" ^ st.turn ^ " BUT conn.username is:" ^
        conn.username) >>
        send_turn_error frame conn
      else
        if String.trim frame.body = "resign" then
          let opp = get_player' conn.username st.players in
          begin
            H.remove state.games conn.username;
            H.remove state.games opp;
            let str_rep = Games.Tictactoe.to_string st.gstate in
            let str_rep' = opp ^ " wins!\n" ^ str_rep in
            let reply = Protocol.make_game_message str_rep' st.players "" in
            let conns = H.find state.map dest in
            send_all conns reply
          end
        else begin
          let gstate' = Games.Tictactoe.play st.gstate frame.body in
          let opp = get_player' conn.username st.players in
          if Games.Tictactoe.is_over gstate' then
            begin
              H.remove state.games conn.username;
              H.remove state.games opp;
              let str_rep = Games.Tictactoe.to_string gstate' in
              let str_rep' = "Game over.\n" ^ str_rep in
              let reply = Protocol.make_game_message str_rep' st.players "" in
              let conns = H.find state.map dest in
              send_all conns reply
            end
          else
            begin
              st.gstate <- gstate';
              st.turn <- opp;
              let str_rep = Games.Tictactoe.to_string gstate' in
              let str_rep' = (opp ^ " to move.\n") ^ str_rep in
              let reply = Protocol.make_game_message str_rep' st.players "" in
              let conns = H.find state.map dest in
              send_all conns reply
            end
          end
      else
        let fr = Protocol.make_error "game" "Invalid header in GAME frame by client." in
        Protocol.send_frame fr conn.output
  with
  (* if not found that means, user tried to #play <cmd-str> without an active *)
  (* game going on for them *)
  | Not_found ->
    let fr = Protocol.make_error "game" "There is no active game for you." in
    Protocol.send_frame fr conn.output
  | Games.Tictactoe.Invalid_move ->
    let fr = Protocol.make_error "game" "Invalid move." in
    Protocol.send_frame fr conn.output
  | Invalid_argument ex ->
    Lwt_log.info ex >>
    let fr = Protocol.make_error "game" "Invalid move." in
    Protocol.send_frame fr conn.output

let handle_frame frame conn =
  Lwt_log.info "in handle frame" >>
  match frame.cmd with
  | SEND -> Lwt_log.info "Received a SEND frame" >>= fun _ ->
            handle_send frame conn
  | SUBSCRIBE -> Lwt_log.info "Received an SUBSCRIBE frame" >>= fun _ ->
                 handle_subscribe frame conn
  | UNSUBSCRIBE -> Lwt_log.info "Received an UNSUB farme" >>= fun _ ->
                  handle_unsubscribe frame conn
  | DISCONNECT -> Lwt_log.info "DISCONNECTING a client" >>= fun _ ->
                  handle_disconnect frame conn
  | GAME -> Lwt_log.info "Received a GAME frame" >>= fun _ ->
            handle_game frame conn
  | _ -> fail (Failure "invalid client frame")

let handle_connection conn () =
  try_lwt
    let rec loop () =
      lwt frame = Protocol.read_frame conn.input in
      handle_frame frame conn >>
      loop ()
    in
      loop ()
  with End_of_file ->
    (* incl CTRL-C  *)
    let dummy = Protocol.make_disconnect in
    handle_disconnect dummy conn

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
    match fr.cmd with
    | CONNECT ->
      begin
        (* let reply = make_connected (string_of_int (newi ()) ) in *)
        let reply = {
          cmd = Protocol.CONNECTED;
          headers = ["session",string_of_int (newi ())];
          body = "";
        } in
        let init_stats = Protocol.make_stats "num_in_rooms" (room_nums ()) in
        let username = List.assoc "login" fr.headers in
        if String.length username > 9 || String.length username < 1 then
          let reply = make_error "Invalid Nickname"
           "Nickname must be between 1 to 9 characters long." in
          Protocol.send_frame reply oc >> Lwt_io.abort ic
        else
          if H.mem state.user_map username then
            let reply = make_error "" "Username already taken" in
            Protocol.send_frame reply oc >> Lwt_io.abort ic
          else
            (* successful, passed above checks *)
            Lwt_log.info ("user " ^ username ^ " has logged in.") >>= fun _ ->
            let conn = {input = ic; topic = None; output = oc; username = username} in
            state.connections <- CSET.add conn state.connections;
            H.add state.user_map conn.username conn;
            try_lwt
              Protocol.send_frame reply oc >>
              Protocol.send_frame init_stats oc >>= fun () ->
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
  Lwt_log.info ("client connected of id" ^ client_id) >>
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  establish_connection ic oc client_id

(*
 * [create_socket () ] creates a socket of type stream in the internet
 * domain with the default protocol and returns it
*)
let create_socket port () =
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
let create_server port () =
  print_endline "Creating socket\n";
  let server_socket = create_socket port () in
  let rec serve () =
    let client = Lwt_unix.accept server_socket in
    client >>= accept_connection >>= serve
  in serve

(* initialize the server *)
let run_server port debug =
  (* enable logging up to the INFO level (default = true) *)
  let () = if debug then Lwt_log.add_rule "*" Lwt_log.Info
           else Lwt_log.add_rule "*" Lwt_log.Error in
  print_endline "Running server\n";
  clean_state ();
  let serve = create_server port () in
  Lwt_main.run @@ serve ()

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
