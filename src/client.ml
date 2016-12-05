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

let rec read_nickname () =
  ANSITerminal.(print_string [cyan]
                  "\nPlease choose a nickname or username.\n");
  print_string "username: ";
  let nick = read_line () in
  if String.length nick > 9 || String.length nick < 1 then
    begin print_endline "Nickname is too short or long";
    read_nickname () end
  else
    nick

let start_connection login pass servFromChannel servToChannel=
  let conframe = Protocol.make_connect login pass in
  let newconn = {input = servFromChannel;
                 output = servToChannel;
                 topic = None;
                 username = login} in
  cur_connection := newconn;
  Protocol.send_frame conframe newconn.output

let port=9000
(* we're using the same port on the host machine and on the server*)
let backlog = 100

let option_to_str s=
  match s with
  |Some x -> x
  |None -> ""

let handle_leave cur_topic=
  lwt ()=Lwt_log.info ("Current room is "^(option_to_str (!cur_connection).topic)) in
  let unsubframe=make_unsubscribe cur_topic in
  Protocol.send_frame unsubframe (!cur_connection).output

let handle_quit () =
  print_endline "Quitting the application\n";
  let disconframe = make_disconnect in
  Protocol.send_frame disconframe (!cur_connection).output

let handle_change nroom cur_topic=
  let unsubframe = make_unsubscribe cur_topic in
  let subframe = make_subscribe nroom in
  (!cur_connection).topic <- Some nroom;
  send_frame unsubframe (!cur_connection).output >>
  send_frame subframe (!cur_connection).output

let handle_join nroom=
  print_endline ("Attempting to join room "^nroom^"\n");
  let subframe = make_subscribe nroom in
  let ()=update_topic nroom in
  send_frame subframe (!cur_connection).output

let handle_send msg cur_topic : unit Lwt.t =
  let sendframe = make_send cur_topic msg in
  lwt () = Lwt_log.info "About to send the sendframe" in
  send_frame sendframe (!cur_connection).output

let handle_game_client_side game_msg cur_topic =
  let sender = (!cur_connection).username in
  let gameframe = Protocol.make_game cur_topic game_msg sender in
  send_frame gameframe (!cur_connection).output

let print_to_gui display_str=
  Notty.I.string (Notty.A.fg Notty.A.cyan) display_str |>
  Notty_lwt.output_image_endline >>= fun () ->
  return (Gui_helper.msg_insert "" display_str)

(* print stats, print error, message to private*)
let rec_stats fr =
  (* headers of stats frame is an assoc list of Topics x num subscribers *)
  let hdrs=fr.headers in
  let rec helper hdrs=
    match hdrs with
    |[]-> return ()
    |(top,numsub)::t->
      let display_str = " < " ^ top ^ " > room has " ^ numsub ^ " users " ^ fr.body in
      lwt()= print_to_gui display_str
      in
      helper t
  in helper hdrs


let rec_error fr =
  let short = Protocol.get_header fr "message" in
  print_to_gui ("ERROR: "^short)

let rec_message fr =
  let sender = Protocol.get_header fr "sender" in
  let mid = Protocol.get_header fr "message-id" in
  let display_str = " < " ^ mid ^ " > " ^ sender ^ " : " ^ fr.body in
  print_to_gui display_str

let rec_game_message fr =
  let instructions = Protocol.get_header  fr "instructions" in
  Lwt_log.info instructions >> Lwt_log.info fr.body

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

let rec repl () =
  lwt () = Lwt_log.info "in repl" in
  lwt raw_input = Lwt_io.read_line Lwt_io.stdin in
  let cur_topic = option_to_str ((!cur_connection).topic) in
  if Str.string_match dir_re raw_input 0 then
    let wdlst = Str.split (Str.regexp "[ \t]+") raw_input in
    match wdlst with
    | [dir] ->
        if dir = "#quit" then handle_quit ()
        else if dir = "#leave" then handle_leave cur_topic
        else Lwt_io.print "Invalid directive" >> repl ()
    | [dir;topic] ->
        (* TODO: games *)
        if String.length topic > 50 || String.length topic < 1 then
          Lwt_io.print "Room name is not valid (Must be between 1 and 50 characters).\n"
          >> repl ()
        else
          begin
            if dir = "#join" then handle_join topic >> repl ()
            else if dir = "#change" then handle_change topic cur_topic >> repl ()
            else Lwt_io.print "Invalid directive command" >> repl ()
          end
    | _ ->
        Lwt_io.print "Invalid directive command" >> repl ()
  else
    Lwt_log.info "Attempting to send message" >>
    handle_send raw_input cur_topic  >>
    Lwt_log.info "Sent a frame" >> repl ()


let rec process raw_input =
  let cur_topic = option_to_str ((!cur_connection).topic) in
  if Str.string_match dir_re raw_input 0 then
    let wdlst = Str.split (Str.regexp "[ \t]+") raw_input in
    match wdlst with
    | [dir] ->
        if dir = "#quit" then handle_quit ()
        else if dir = "#leave" then handle_leave cur_topic
        else Lwt_io.print "Invalid directive"
    | [dir;topic] ->
        (* TODO: games *)
        if String.length topic > 50 || String.length topic < 1 then
          Lwt_io.print "Room name is not valid (Must be between 1 and 50 characters).\n"
        else
          begin
            if dir = "#join" then handle_join topic
            else if dir = "#change" then handle_change topic cur_topic
            else Lwt_io.print "Invalid directive command"
          end
    | _ ->
        Lwt_io.print "Invalid directive command"
  else
    Lwt_log.info "Attempting to send message" >>
    handle_send raw_input cur_topic >>
    Lwt_log.info "Sent a frame"


let handle_connection () =
  let rec loop () =
    handle_incoming_frames () >>= loop
  in
  loop ()

let main ipstring =
  try_lwt
    let inet_addr : Lwt_unix.inet_addr = Unix.inet_addr_of_string ipstring in
    let addr = Lwt_unix.ADDR_INET (inet_addr,port) in
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    (*Do not need to bind, it is implicitly done - google this*)
    lwt () = Lwt_unix.connect sock addr in
    let oc = Lwt_io.of_fd Lwt_io.Output sock in
    let ic = Lwt_io.of_fd Lwt_io.Input sock in
    print_endline "right before read pw";
    let login = read_nickname () in
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
