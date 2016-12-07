(*
 * client.ml
 * Copyright (C) 2016
 * sb892 <sb892@cornell.edu> Somrita Banerjee,
 * yq56 <yq56@cornell.edu> Yuhuan Qiu,
 * ew366 <ew366@cornell.edu> Eric Wang,
 * bl458 <bl458@cornell.edu> Byungchan Lim
 *
 * Distributed under terms of the MIT license.
*)

open Unix
open Lwt
open Protocol
open Games

(* type [connection] is the connection between server and client*)
type connection = {
  input      : Lwt_io.input_channel;
  output     : Lwt_io.output_channel;
  (* Can only be subscribed to one topic or i.e. be in one chatroom at a time *)
  mutable topic : string option;
  username   : string
}

(* type [message] is the format of all messages passed*)
type message = {
  id : float; (* The timestamp of the message. Unique identifier for messages
  with the same destination. *)
  conn : connection;
  content : string
}

(* Syntactic sugar for anonymous binding*)
let (>>) (dt : unit Lwt.t) f = dt >>= (fun () -> f)

(*initialize client channel to an output that drops everything, and an input
that never reads *)
let (emptyconn:connection)= {
  input  = Lwt_io.zero;
  output = Lwt_io.null;
  topic  = None; username=""
}

(* [cur_connection] is the ref to the connection object for this client*)
let cur_connection = ref emptyconn

(* [update_topic] changes the topic (chatroom) of this connection to [top]*)
let update_topic top =
  (!cur_connection).topic <- Some top

(* [remove_Topic] changes the topic (chatroom) of this connection to nothing*)
let remove_topic () =
  (!cur_connection).topic <- None

(* [start_connection] initializes the connection using a login, output and
input channels*)
let start_connection login pass servFromChannel servToChannel=
  let conframe = Protocol.make_connect login pass in
  let newconn = {input = servFromChannel;
                 output = servToChannel;
                 topic = None;
                 username = login} in
  cur_connection := newconn;
  Protocol.send_frame conframe newconn.output

let backlog = 100

(* [option_to_str] converts a string option to string*)
let option_to_str s=
  match s with
  |Some x -> x
  |None -> ""

(* [shorter_room_name] converts a room name of type /topic/Cornell to Cornell*)
let shorter_room_name s=
  let slist = Str.split (Str.regexp "[/]+") s in
  List.nth slist 1

(* [print_to_gui] is a wrapper for Gui_helper.msg_insert. Refer to that
 * function for documentation*)
let print_to_gui ?msg_type:(msg_type=`NORMAL) ?is_game:(game_bool=false)
                 identifier display_str=
  Lwt_io.print display_str >>
  return (Gui_helper.msg_insert ~msg_type:msg_type ~is_game:game_bool
                                identifier display_str)

(* [handle_leave] sends out an unsubscribe frame when user wants to leave room*)
let handle_leave cur_topic=
  lwt ()=Lwt_log.info ("Current room is "^(option_to_str
  (!cur_connection).topic)) in
  let unsubframe=make_unsubscribe cur_topic in
  let ()=Gui_helper.set_usr_list [] in
  let ()=Gui_helper.set_room_label "" in
  let ()=remove_topic () in
  Protocol.send_frame unsubframe (!cur_connection).output

(* [handle_quit] sends out a disconnect frame when user wants to quit app*)
let handle_quit () =
  Lwt_log.info "Quitting the application" >>
  let disconframe = make_disconnect in
  Protocol.send_frame disconframe (!cur_connection).output


(* [handle_change] sends out an unsubscribe frame and a subscribe frame when
user wants to switch from current room to [nroom]*)
let handle_change nroom cur_topic=
  let unsubframe = make_unsubscribe cur_topic in
  let subframe = make_subscribe nroom in
  let ()=Gui_helper.set_room_label (shorter_room_name nroom) in
  (!cur_connection).topic <- Some nroom;
  send_frame unsubframe (!cur_connection).output >>
  send_frame subframe (!cur_connection).output

(* [handle_join] sends out a subscribe frame when user wants to join [nroom]*)
let handle_join nroom=
  lwt ()=Lwt_log.info ("Attempting to join room "^nroom^"\n") in
  let subframe = make_subscribe nroom in
  let ()=update_topic nroom in
  let ()=Gui_helper.set_room_label (shorter_room_name nroom) in
  send_frame subframe (!cur_connection).output

(* [handle_send] sends out a send frame when user messages a room*)
let handle_send msg cur_topic : unit Lwt.t =
  let sendframe = make_send cur_topic msg in
  lwt () = Lwt_log.info "About to send the sendframe" in
  send_frame sendframe (!cur_connection).output

(* [handle_private_message] sends out a send frame when user messages one
person [uname] privately*)
let handle_private_message uname msg=
  let pri_topic="/private/"^uname in
  let sendframe = make_send pri_topic msg in
  lwt () = Lwt_log.info "About to send the private sendframe" in
  send_frame sendframe (!cur_connection).output

(* [handle_play] sends out a game frame when user wants to play a game*)
let handle_play ?(opp=None) challenge cmd cur_topic =
  (* dest opp game_cmd *)
  match opp with
  | None ->
      let fr = Protocol.make_game cur_topic challenge "" cmd in
      Protocol.send_frame fr (!cur_connection).output
  | Some o ->
      let fr = Protocol.make_game cur_topic challenge o cmd in
      Protocol.send_frame fr (!cur_connection).output

(*[rec_stats] deals with printing the number of people in each room, or the list
of poeple in each room when the client gets a STATS frame*)
let rec_stats fr =
  (* headers of stats frame is an assoc list of Topics x num subscribers *)
  (*let hdrs=fr.headers in*)
  let type_of_stats=Protocol.get_header fr "type" in
  let hdrs= List.remove_assoc "type" fr.headers in
  let rec helper hdrs=
    match hdrs with
    (* ALL PRINTS HERE SHOULD BE THE COLOR OF status of rooms and num users*)
    |[]-> let display_str=" To join a room, type in #join [room name]" in
          lwt ()= print_to_gui ~msg_type:`STATUS "" display_str in
          return ()
    |(roomtop,numsub)::t->
      let top=String.sub roomtop 7 ((String.length roomtop)-7) in
      let display_str = " " ^ top ^ " room has " ^ numsub ^ " users " in
      lwt ()= print_to_gui ~msg_type:`STATUS "" display_str
      in
      helper t
  in
  if (String.equal type_of_stats "num_in_rooms") then
    helper hdrs
  else if (String.equal type_of_stats "room_inhabitants") then
    match hdrs with
    |[]-> Lwt_log.info "Error! No inhabitant info"
    |(k,v)::t->
      let userlist = Str.split (Str.regexp "[,]+") v in
      return (Gui_helper.set_usr_list userlist)
  else
    Lwt_log.info "type not recognized"

(* [rec_error] deals with printing error to the gui when the client gets an
ERROR frame*)
let rec_error fr =
  (* ALL PRINTS HERE SHOULD BE RED*)
  lwt ()=Lwt_log.info "Trying to print the error" in
  let errorbody=fr.body in
  print_to_gui ~msg_type:`ERROR "" ("ERROR: "^ " "^errorbody )

(* [current_time] provides a time stamp in hours,min,sec*)
let current_time ()=
  let unixtime=Unix.localtime (Unix.gettimeofday ()) in
  let hr=string_of_int unixtime.tm_hour in
  let min=string_of_int unixtime.tm_min in
  let sec=string_of_int unixtime.tm_sec in
  hr^":"^min^":"^sec

(* [rec_message] deals with printing the message to the gui when the client gets
a MESSAGE frame*)
let rec_message fr =
  let sender = Protocol.get_header fr "sender" in
  (* let mid = Protocol.get_header fr "message-id" in *)
  let mid = current_time () in
  let dest=Protocol.get_header fr "destination" in
  (* ALL PRINTS HERE SHOULD BE color-coded. One if destination is private,
  one if sender is Server *)
  if (String.equal sender "SERVER") then
    let id_str = " < " ^ mid ^ " > " ^ sender ^ " : "in
    let msg = fr.body in
    print_to_gui ~msg_type:`SERVER id_str msg
  else if (String.equal (String.sub dest 0 9) "/private/")
    then
    let recip = Str.string_after dest 9 in
    let sender' = if !(cur_connection).username = sender then ("To " ^ recip)
       else sender in
    let id_str = " < " ^ mid ^ " > " ^ sender' ^ " : " in
    let msg = fr.body in
    print_to_gui ~msg_type:`PM  id_str msg
  else
    let display_str = " < " ^ mid ^ " > " ^ sender ^ " : " in
    let msg = fr.body in
    print_to_gui display_str msg

(* [rec_gmessage] deals with the activities associated with a game when the
cleint gets a GAME frame*)
let rec_gmessage fr =
  (* ALL PRINTS HERE SHOULD BE game colors*)
  (* instructions repld, see #help *)
  let players = (Protocol.get_header fr "player1") ^ " vs " ^
  (Protocol.get_header fr "player2")  in
  let id_str = " < " ^ (current_time ()) ^ " > " ^ players ^ " :\n" in
  let msg = fr.body in
  Lwt_io.print (id_str^msg) >>
  return (Gui_helper.msg_insert id_str msg)

(* [handle_incoming_frames] is a continuously executing function to deal with
incoming frames *)
let rec handle_incoming_frames ()=
  let ic = (!cur_connection).input in
  Protocol.read_frame ic >>= fun fr ->
  match fr.cmd with
  | MESSAGE-> Lwt_log.info ("received message body: " ^ fr.body)
    >> rec_message fr
  | ERROR-> Lwt_log.info "received ERROR frame" >>
            rec_error fr
  | STATS -> Lwt_log.info "received STATS frame" >>
             rec_stats fr
  | GAME_RESP -> Lwt_log.info ("received GAME_RESP body: " ^ fr.body)
    >> rec_gmessage fr
  | _ -> Lwt_log.info ("received a frame of type not expected")

(* LIST OF DIRECTIVES
   [#change nrooom] changes room to nroom (unsubscribe and subscribe)
   [#leave] leaves room (unsubscribe)
   [#join nroom] joins a new room (requires not in any room currently)
   [#game game_msg] plays a game
   [#chatbot] changes to chatbot room
   [#pm user msg] sends the msg to only the user
   [#quit] closes the connection to server
   Note: only change, leave, join, quit, game implemented
   Note: for tictactoe, string game_msg is in the form:
   opponent_name ^ " " ^ game_cmd *)

(* Regex for start of every directive*)
let dir_re = Str.regexp "#"

(*[is_valid_rmname] checks validity of room name*)
let is_valid_rmname topic =
  if String.length topic > 50 || String.length topic < 1 then false else true

(*[is_valid_uname] checks validity of user name*)
let is_valid_uname topic =
  if String.length topic > 9 || String.length topic < 1 then false else true

(* not aligned in gui correctly because of not monospaced font *)
let help = "
Directives:
#quit                           Quit the application
#join <room>                    Joins <room> (Must be in a lobby)
#change <room>                  Changes current room to <room> (Must be in a room)
#leave                          Leaves the current room (Must already be in a room)
#pm <nickname>                  Sends a private message to this user
#play challenge <username>      Starts a tictactoe game with <username>
#play i,j                       Plays an X | O at row i, column j
#play resign                    Resign
#help                           Display this message
"

let handle_help () =
  (* ALL PRINTS HERE SHOULD BE color of help, same as server??*)
  print_to_gui ~msg_type:`SERVER "" help

(* [process] handles the raw_input from the user and sends the right frames*)
let rec process raw_input =
  let cur_topic = option_to_str ((!cur_connection).topic) in
  if Str.string_match dir_re raw_input 0 then
    let wdlst = Str.split (Str.regexp "[ \t]+") raw_input in
    match wdlst with
    | [dir] ->
        if dir = "#quit" then handle_quit ()
        else if dir = "#leave" then handle_leave cur_topic
        else if dir = "#help" then handle_help ()
        else print_to_gui ~msg_type:`ERROR "" "Error: Invalid directive."
    | [dir;arg1] ->
          begin
            if dir = "#join" then
              if is_valid_rmname arg1 then
                let rmname=("/topic/"^arg1) in
                handle_join rmname
              else
                print_to_gui ~msg_type:`ERROR "" ("Room name is not valid" ^
                "Must be between 1 and 50 characters.\n")
            else
              begin match !(cur_connection).topic with
              | None ->
                print_to_gui ~msg_type:`ERROR "" "Error: Invalid directive."
              | Some t ->
                if dir = "#change" then
                  if is_valid_rmname arg1 then
                    handle_change ("/topic/"^arg1) cur_topic
                  else
                    print_to_gui ~msg_type:`ERROR "" ("Room name is not valid" ^
                    "Must be between 1 and 50 characters.\n")
                else if dir = "#play" then
                  (* TODO: resign *)
                  handle_play "false" arg1 cur_topic
                else
                  print_to_gui ~msg_type:`ERROR "" ("Invalid directive.")
              end
          end
(* let handle_play ?(opp=None) challenge cmd cur_topic = *)
    | [dir;arg1;arg2] ->
        begin match !(cur_connection).topic with
        | None ->
          print_to_gui ~msg_type:`ERROR "" "Error: Invalid directive."
        | Some t ->
          if dir = "#play" && arg1 = "challenge" then
            handle_play ~opp:(Some arg2) "true" "" cur_topic
          else if dir= "#pm" then
            if (is_valid_uname arg1) then
              handle_private_message arg1 arg2
            else
              print_to_gui ~msg_type:`ERROR "" ("Username is not valid ^
              (Must be between 1 and 9 characters).\n")
          else
            print_to_gui ~msg_type:`ERROR "" "Error: Invalid directive."
        end
    | dir::arg1::t ->
      if dir= "#pm" then
        if (is_valid_uname arg1) then
          let arg2=String.concat " " t in
          handle_private_message arg1 arg2
        else
          print_to_gui ~msg_type:`ERROR "" ("Username is not valid ^
          (Must be between 1 and 9 characters).\n")
      else
        print_to_gui ~msg_type:`ERROR "" ("Invalid directive.")
    |_->
      print_to_gui ~msg_type:`ERROR "" ("Invalid directive.")
  else
    begin match !(cur_connection).topic with
    | None ->
      print_to_gui ~msg_type:`ERROR ""
                  "Error: You must be in a room to send a message"
    | Some t ->
      handle_send raw_input cur_topic end

(* [handle_connection] is the looping function that makes it possible to handle
incoming frames*)
let handle_connection () =
  let rec loop () =
    handle_incoming_frames () >>= loop
  in
  loop ()

(* [main] is the main method for client.ml which establishes a connection and
gets the loop started*)
let main (ipstring:string) (login:string) (port:int) =
  try_lwt
    let inet_addr : Lwt_unix.inet_addr = Unix.inet_addr_of_string ipstring in
    let addr = Lwt_unix.ADDR_INET (inet_addr,port) in
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect sock addr in
    let oc = Lwt_io.of_fd Lwt_io.Output sock in
    let ic = Lwt_io.of_fd Lwt_io.Input sock in
    let f = fun x->
      match x.cmd with
      | CONNECTED->
        Lwt_log.info "recieved CONNECTED frame from server"
      | _->
        Lwt_log.info "expected a CONNECTED frame but got something else" in
    start_connection login "" ic oc >>= fun () ->
    Protocol.read_frame ic >>= f >>= fun fr ->
    handle_connection ()
  with
  | Failure _ ->
    Lwt_log.info "\n\nError.Malformed IP Address.\n"
  | _ ->
    Lwt_log.info "Some other error"

let () = Lwt_log.add_rule "*" Lwt_log.Info
