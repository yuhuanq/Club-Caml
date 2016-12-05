(*
 * client.ml
 * Copyright (C) 2016
 * sb892 <sb892@cornell.edu> Somrita Banerjee,
 * ew366 <ew366@cornell.edu> Eric Wang
 * bl458 <bl458@cornell.edu> Byungchan Lim
 *
 * Distributed under terms of the MIT license.
*)

(* Reminder:
   1. need to do code for when client receives a game_resp frame from server.
   2. client.mli??
   3. database frame to send to server to request data. In this case, chat
   history*)

open Unix
open Lwt
open Protocol
open Games

type connection = {
  input      : Lwt_io.input_channel;
  output     : Lwt_io.output_channel;
  (* Can only be subscribed to one topic or i.e. be in one chatroom at a time *)
  mutable topic : string option;
  username   : string
}

type message = {
  id : float; (* The timestamp of the message. Unique identifier for messages with the same destination. *)
  conn : connection;
  content : string
}

let (>>) (dt : unit Lwt.t) f = dt >>= (fun () -> f)

(*initialize client channel to an output that drops everything*)
(*type client_channel= output_channel ref
  let cur_channel
  let (client_channel:output_channel)= null
*)

let (emptyconn:connection)= {
  input  = Lwt_io.zero;
  output = Lwt_io.null;
  topic  = None; username=""
}

let cur_connection = ref emptyconn

let update_topic top =
  (!cur_connection).topic <- Some top

let remove_topic () =
  (!cur_connection).topic <- None

let start_connection login pass servFromChannel servToChannel=
  let conframe = Protocol.make_connect login pass in
  let newconn = {input = servFromChannel;
                 output = servToChannel;
                 topic = None;
                 username = login} in
  cur_connection := newconn;
  Protocol.send_frame conframe newconn.output

let backlog = 100

let option_to_str s=
  match s with
  |Some x -> x
  |None -> ""

let shorter_room_name s=
  let slist = Str.split (Str.regexp "[/]+") s in
  List.nth slist 1

let print_to_gui display_str=
  (* Notty.I.string (Notty.A.fg Notty.A.cyan) display_str |> *)
  (* Notty_lwt.output_image >>= fun () -> *)
  Lwt_io.print display_str >>
  return (Gui_helper.msg_insert "" display_str)

let handle_leave cur_topic=
  lwt ()=Lwt_log.info ("Current room is "^(option_to_str (!cur_connection).topic)) in
  let unsubframe=make_unsubscribe cur_topic in
  let ()=Gui_helper.set_usr_list [] in
  let ()=Gui_helper.set_room_label "" in
  let ()=remove_topic () in
  Protocol.send_frame unsubframe (!cur_connection).output

let handle_quit () =
  print_endline "Quitting the application\n";
  let disconframe = make_disconnect in
  Protocol.send_frame disconframe (!cur_connection).output

let handle_change nroom cur_topic=
  let unsubframe = make_unsubscribe cur_topic in
  let subframe = make_subscribe nroom in
  let ()=Gui_helper.set_room_label (shorter_room_name nroom) in
  (!cur_connection).topic <- Some nroom;
  send_frame unsubframe (!cur_connection).output >>
  send_frame subframe (!cur_connection).output

let handle_join nroom=
  lwt ()=Lwt_log.info ("Attempting to join room "^nroom^"\n") in
  let subframe = make_subscribe nroom in
  let ()=update_topic nroom in
  let ()=Gui_helper.set_room_label (shorter_room_name nroom) in
  send_frame subframe (!cur_connection).output

let handle_send msg cur_topic : unit Lwt.t =
  let sendframe = make_send cur_topic msg in
  lwt () = Lwt_log.info "About to send the sendframe" in
  send_frame sendframe (!cur_connection).output

let handle_private_message uname msg=
  let pri_topic="/private/"^uname in
  let sendframe = make_send pri_topic msg in
  lwt () = Lwt_log.info "About to send the private sendframe" in
  send_frame sendframe (!cur_connection).output

let handle_play ?(opp=None) challenge cmd cur_topic =
  (* dest opp game_cmd *)
  match opp with
  | None ->
      let fr = Protocol.make_game cur_topic challenge "" cmd in
      Protocol.send_frame fr (!cur_connection).output
  | Some o ->
      let fr = Protocol.make_game cur_topic challenge o cmd in
      Protocol.send_frame fr (!cur_connection).output

(* print stats, print error, message to private*)
let rec_stats fr =
  (* headers of stats frame is an assoc list of Topics x num subscribers *)
  (*let hdrs=fr.headers in*)
  let type_of_stats=Protocol.get_header fr "type" in
  let hdrs= List.remove_assoc "type" fr.headers in
  let rec helper hdrs=
    match hdrs with
    |[]-> let display_str=" To join a room, type in #join [room name]" in
          lwt ()= print_to_gui display_str in
          return ()
    |(roomtop,numsub)::t->
      let top=String.sub roomtop 7 ((String.length roomtop)-7) in
      let display_str = " " ^ top ^ " room has " ^ numsub ^ " users " in
      lwt ()= print_to_gui display_str
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
    Lwt_log.info "to be implemented"


let rec_error fr =
  (*let short = Protocol.get_header fr "message" in*)
  lwt ()=Lwt_log.info "Trying to print the error" in
  let errorbody=fr.body in
  print_to_gui ("ERROR: "^ " "^errorbody )

let rec_message fr =
  let sender = Protocol.get_header fr "sender" in
  let mid = Protocol.get_header fr "message-id" in
  let display_str = " < " ^ mid ^ " > " ^ sender ^ " : " ^ fr.body in
  print_to_gui display_str


let rec_gmessage fr =
  (* instructions may = "" *)
  let instructions = Protocol.get_header fr "instructions" in
  let players = (Protocol.get_header fr "player1") ^ " vs " ^ (Protocol.get_header fr
  "player2")  in
  let display_str = " < " ^ (string_of_float (Unix.gettimeofday ())) ^ " > " ^ players ^ " :\n" ^ fr.body in
  (*
   * Notty.I.string (Notty.A.fg Notty.A.cyan) instructions |>
   * Notty_lwt.output_image_endline >>= fun () ->
   * Gui_helper.msg_insert "" display_str;
   *)
  (* Notty.I.string (Notty.A.fg Notty.A.cyan) display_str |> *)
  (* Notty_lwt.output_image_endline >>= fun () -> *)
  Lwt_io.print display_str >>
  return (Gui_helper.msg_insert "" display_str)


(* TODO: handle incoming messages*)
let rec handle_incoming_frames ()=
  lwt () = Lwt_log.info "Inside handle_incoming_frames" in
  let ic = (!cur_connection).input in
  Protocol.read_frame ic >>= fun fr ->
  match fr.cmd with
  | MESSAGE-> Lwt_log.info "received MESSAGE frame" >>
    Lwt_log.info ("received message body: " ^ fr.body)
    >> rec_message fr
  | ERROR-> Lwt_log.info "received ERROR frame" >>
            rec_error fr
  | STATS -> Lwt_log.info "received STATS frame" >>
             rec_stats fr
  | GAME_RESP -> Lwt_log.info "received GAME_RESP frame."
    >> Lwt_log.info ("received GAME_RESP body: " ^ fr.body)
    >> rec_gmessage fr
  | _ -> Lwt_log.info ("received a frame of type not expected")


(* [#change nrooom] changes room to nroom (unsubscribe and subscribe)
   [#leave] leaves room (unsubscribe)
   [#join nroom] joins a new room (requires not in any room currently)
   [#game game_msg] plays a game
   [#chatbot] changes to chatbot room
   [#quit] closes the connection to server
   Note: only change, leave, join, quit, game implemented
   Note: for tictactoe, string game_msg is in the form:
   opponent_name ^ " " ^ game_cmd *)

let dir_re = Str.regexp "#"

let is_valid_rmname topic =
  if String.length topic > 50 || String.length topic < 1 then false else true

let is_valid_uname topic =
  if String.length topic > 9 || String.length topic < 1 then false else true

let rec repl () =
  lwt () = Lwt_log.info "in repl" in
  lwt raw_input = Lwt_io.read_line Lwt_io.stdin in
  let cur_topic = option_to_str ((!cur_connection).topic) in
  if Str.string_match dir_re raw_input 0 then
    let wdlst = Str.split (Str.regexp "[ \t]+") raw_input in
    match wdlst with
    | [dir] ->
        Lwt_io.print "in dir mc" >>
        if dir = "#quit" then handle_quit ()
        else if dir = "#leave" then handle_leave cur_topic
        else Lwt_io.print "Invalid directive" >> repl ()
    | [dir;arg1] ->
        (* TODO: games *)
          Lwt_io.print "in dir;arg1 match case" >>
          begin
            if dir = "#join" then
              if is_valid_rmname arg1 then
                let rmname=("/topic/"^arg1) in
                handle_join rmname >> repl ()
              else
                Lwt_io.print "Room name is not valid (Must be between 1 and 50 characters).\n"
                >> repl ()
            else if dir = "#change" then
              if is_valid_rmname arg1 then
                handle_change ("/topic/"^arg1) cur_topic >> repl ()
              else
                Lwt_io.print "Room name is not valid (Must be between 1 and 50 characters).\n"
                >> repl ()
            else if dir = "#play" then
              (* TODO: resign *)
              handle_play "false" arg1 cur_topic >> repl ()
            else
              Lwt_io.print "Invalid directive command" >> repl ()
          end
(* let handle_play ?(opp=None) challenge cmd cur_topic = *)
    | [dir;arg1;arg2] ->
        begin
          Lwt_io.print "in dir;arg1;arg2 match case" >>
          if dir = "#play" && arg1 = "challenge" then
            handle_play ~opp:(Some arg2) "true" "" cur_topic >> repl ()
          else if dir= "#pm" then
            if (is_valid_uname arg1) then
              handle_private_message arg1 arg2>>repl ()
            else
              Lwt_io.print "User name is not valid (Must be between 1 and 9 characters).\n"
              >> repl ()
          else
            Lwt_io.print "Invalid directive command" >> repl ()
        end
    | dir::arg1::t ->
      Lwt_io.print "in 3 arg mc" >>
      if dir= "#pm" then
        if (is_valid_uname arg1) then
          let arg2=String.concat " " t in
          handle_private_message arg1 arg2>>repl ()
        else
          Lwt_io.print "User name is not valid (Must be between 1 and 9 characters).\n"
          >> repl ()
      else
        Lwt_io.print "Invalid directive command" >> repl ()
    |_->
      Lwt_io.print "in _ mc" >>
      Lwt_io.print "Invalid directive command" >> repl ()

  else
    Lwt_log.info "Attempting to send message" >>
    handle_send raw_input cur_topic >>
    Lwt_log.info "Sent a frame"

let help = "
Directives:
#quit                           Quit the application
#join <room>                    Joins <room> (Must be in a lobby)
#change <room>                  Changes current room to <room> (Must already be in a room)
#leave                          Leaves the current room (Must already be in a room)
#pm <nickname>                  Sends a private message to this user
#play challenge <username>      Starts a tictactoe game with <username>
#play i,j                       Plays an X | O at row i, column j
#play resign                    Resign
#help                           Display this message
"

let handle_help () =
  print_to_gui help

let rec process raw_input =
  let cur_topic = option_to_str ((!cur_connection).topic) in
  if Str.string_match dir_re raw_input 0 then
    let wdlst = Str.split (Str.regexp "[ \t]+") raw_input in
    match wdlst with
    | [dir] ->
        Lwt_io.print "in dir mc" >>
        if dir = "#quit" then handle_quit ()
        else if dir = "#leave" then handle_leave cur_topic
        else if dir = "#help" then handle_help ()
        else Lwt_io.print "Invalid directive"
    | [dir;arg1] ->
        (* TODO: games *)
          Lwt_io.print "in dir;arg1 match case" >>
          begin
            if dir = "#join" then
              if is_valid_rmname arg1 then
                let rmname=("/topic/"^arg1) in
                handle_join rmname >> repl ()
              else
                Lwt_io.print "Room name is not valid (Must be between 1 and 50 characters).\n"
                >> repl ()
            else
              begin match !(cur_connection).topic with
              | None ->
                print_to_gui "Error: Invalid directive."
              | Some t ->
                if dir = "#change" then
                  if is_valid_rmname arg1 then
                    handle_change ("/topic/"^arg1) cur_topic >> repl ()
                  else
                    Lwt_io.print "Room name is not valid (Must be between 1 and 50 characters).\n"
                    >> repl ()
                else if dir = "#play" then
                  (* TODO: resign *)
                  handle_play "false" arg1 cur_topic >> repl ()
                else
                  Lwt_io.print "Invalid directive command" >> repl () end
          end
(* let handle_play ?(opp=None) challenge cmd cur_topic = *)
    | [dir;arg1;arg2] ->
        begin match !(cur_connection).topic with
        | None ->
          print_to_gui "Error: Invalid directive."
        | Some t ->
          if dir = "#play" && arg1 = "challenge" then
            handle_play ~opp:(Some arg2) "true" "" cur_topic >> repl ()
          else if dir= "#pm" then
            if (is_valid_uname arg1) then
              handle_private_message arg1 arg2>>repl ()
            else
              Lwt_io.print "User name is not valid (Must be between 1 and 9 characters).\n"
              >> repl ()
          else
            Lwt_io.print "Invalid directive command" >> repl () end
    | dir::arg1::t ->
      Lwt_io.print "in 3 arg mc" >>
      if dir= "#pm" then
        if (is_valid_uname arg1) then
          let arg2=String.concat " " t in
          handle_private_message arg1 arg2>>repl ()
        else
          Lwt_io.print "User name is not valid (Must be between 1 and 9 characters).\n"
          >> repl ()
      else
        Lwt_io.print "Invalid directive command" >> repl ()
    |_->
      Lwt_io.print "in _ mc" >>
      Lwt_io.print "Invalid directive command" >> repl ()
  else
    begin match !(cur_connection).topic with
    | None ->
      print_to_gui "Error: You must be in a room to send a message"
    | Some t ->
      Lwt_log.info "Attempting to send message" >>
      handle_send raw_input cur_topic >>
      Lwt_log.info "Sent a frame" end

let handle_connection () =
  let rec loop () =
    handle_incoming_frames () >>= loop
  in
  loop ()

let main (ipstring:string) (login:string) (port:int) =
  try_lwt
    let inet_addr : Lwt_unix.inet_addr = Unix.inet_addr_of_string ipstring in
    let addr = Lwt_unix.ADDR_INET (inet_addr,port) in
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect sock addr in
    let oc = Lwt_io.of_fd Lwt_io.Output sock in
    let ic = Lwt_io.of_fd Lwt_io.Input sock in
    print_endline "right before read pw";
    let f = fun x->
      match x.cmd with
      | CONNECTED->
        Lwt_log.info "recieved CONNECTED frame from server"
      | _->
        Lwt_log.info "expected a CONNECTED frame but got something else" in
    start_connection login "" ic oc >>= fun () ->
    print_endline "before protocol read_frame in client";
    lwt () = Lwt_log.info "before protocol read_Frame in client" in
    Protocol.read_frame ic >>= f >>= fun fr ->
    handle_connection ()
  with
  | Failure _ ->
    return (ANSITerminal.(print_string [red]
                            "\n\nError. Malformed IP Address.\n"))
  | _ -> return (print_endline "Some other error")

let () = Lwt_log.add_rule "*" Lwt_log.Info
