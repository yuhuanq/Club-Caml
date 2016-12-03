(*
 * client.ml
 * Copyright (C) 2016 sb892 <sb892@cornell.edu> Somrita Banerjee,
   ew366 <ew366@cornell.edu> Eric Wang
   bl458 <bl458@cornell.edu> Byungchan Lim
 *
 * Distributed under terms of the MIT license.
*)

(* Reminder:
   1. need to do code for when client receives a game_resp frame from server.
   2. client.mli??
   3. database frame to send to server to request data. In this case, chat history*)

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

let read_password_and_login () =
  ANSITerminal.(print_string [cyan]
  "\nEnter login and password on seperate lines.\n");
  print_string "username: ";
  let log = read_line () in
  print_string "password ";
  ANSITerminal.(print_string [red] "(WARNING:PLAIN TEXT)");
  print_string ":";
  let pass = read_line () in
  print_string "\n\n";
  (log,pass)

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
  (*let f=function
    |x->
      match x.cmd with
      |STATS -> Lwt_log.info ("STATS frame recvd")
        >> Lwt_log.info ("body of frame recvd: "^x.body)
      (* TODO: print header to user*)
      |_-> Lwt_log.info "expected STATS frame"
  in*)
  Protocol.send_frame unsubframe (!cur_connection).output
  (*>>
  read_frame (!cur_connection).input >>= f*)

let handle_quit () =
  print_endline "Quitting the application\n";
  let disconframe = make_disconnect in
  Protocol.send_frame disconframe (!cur_connection).output

let handle_change nroom cur_topic=
  let unsubframe = make_unsubscribe cur_topic in
  let subframe = make_subscribe nroom in
  let f = function
    |x->
      match x.cmd with
      |STATS-> Lwt_io.print ("STATS frame recvd")>>
        Lwt_io.print ("body of frame recvd: "^x.body)
      (* TODO: print header to user*)
      |_-> Lwt_io.print "expected STATS frame" in
  send_frame unsubframe (!cur_connection).output >>
  read_frame (!cur_connection).input >>= f >>
  send_frame subframe (!cur_connection).output


let handle_join nroom=
  print_endline ("Attempting to join room "^nroom^"\n");
  let subframe = make_subscribe nroom in
  let ()=update_topic nroom in
  send_frame subframe (!cur_connection).output

let handle_send msg cur_topic=
  let sendframe = make_send cur_topic msg in
  lwt () = Lwt_log.info "About to send the sendframe" in
  send_frame sendframe (!cur_connection).output

let handle_game_client_side game_msg cur_topic =
  let sender = (!cur_connection).username in
  let gameframe = Protocol.make_game cur_topic game_msg sender in
  send_frame gameframe (!cur_connection).output

(* TODO: handle incoming messages*)


let rec handle_incoming_frames ()=
  lwt () = Lwt_log.info "Inside handle_incoming_frames" in
  let ic = (!cur_connection).input in
  Protocol.read_frame ic >>= fun fr ->
  match fr.cmd with
  | MESSAGE-> Lwt_log.info "received MESSAGE frame">>
    Lwt_log.info ("body of frame recvd: "^fr.body)
  | ERROR-> Lwt_log.info "received ERROR frame"
  | STATS -> Lwt_log.info "received STATS frame"
  | GAME_RESP -> Lwt_log.info "received GAME_RESP frame."
  | x-> Lwt_log.info ("received a frame of type not expected") >>
    (*lwt ()= Lwt_log.info "Received a frame" in*)
    handle_incoming_frames ()

(* [#change nrooom] changes room to nroom (unsubscribe and subscribe)
   [#leave] leaves room (unsubscribe)
   [#join nroom] joins a new room (requires not in any room currently)
   [#game game_msg] plays a game
   [#chatbot] changes to chatbot room
   [#quit] closes the connection to server
   Note: only change, leave, join, quit, game implemented
   Note: for tictactoe, string game_msg is in the form:
   opponent_name ^ " " ^ game_cmd *)

let rec repl () =
  lwt ()=Lwt_log.info "in repl" in
  lwt directive=Lwt_io.read_line Lwt_io.stdin in
  let cur_topic=option_to_str ((!cur_connection).topic) in
  (let firstletter=directive.[0] in
  match firstletter with
  |'#'->
    begin
      match directive with
      |"#leave" -> handle_leave cur_topic
      |"#quit" ->
        print_endline "matched #quit";
        handle_quit ()
      |"#chatbot" -> failwith "Unimplemented chatbot"
      | _ ->
        let partOfDir = String.sub directive 0 7 in
        begin
          match partOfDir with
          |"#change"->
            let nroom = String.sub directive 8 ((String.length directive)-8) in
            handle_change nroom cur_topic
            >>repl ()
          |_->
            let partOfDir2 = String.sub directive 0 5 in
            begin
              match partOfDir2 with
              |"#join"->
                let nroom = String.sub directive 6 ((String.length directive)-6) in
                print_endline ("joining " ^ nroom);
                handle_join nroom
                >> repl ()
              |"#game" ->
                let game_msg = String.sub directive 6 ((String.length directive)-6) in
                handle_game_client_side game_msg cur_topic
                >> repl ()
              | _ -> failwith "invalid # command"
            end
        end
    end
  | _ ->
    lwt () = Lwt_log.info "Attempting to send message" in
    handle_send directive cur_topic )
    >>
    lwt () =  Lwt_log.info "Sent a frame" in
    repl ()

let handle_connection () =
  let rec loop () =
    handle_incoming_frames () >>= loop
    (* and ()=repl () in *)
    (* Lwt_log.info "Completed both loops?">> loop () *)
  in
  loop ()

let main ipstring =
  try_lwt
    let inet_addr : Lwt_unix.inet_addr = Unix.inet_addr_of_string ipstring in
    let addr = Lwt_unix.ADDR_INET (inet_addr,port) in
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    (*Do not need to bind, it is implicitly done - google this*)
    lwt () = Lwt_unix.connect sock addr in
    (* >>= fun () -> *)
    let oc = Lwt_io.of_fd Lwt_io.Output sock in
    let ic = Lwt_io.of_fd Lwt_io.Input sock in
    print_endline "right before read pw";
    let (login,pass) = read_password_and_login () in
    let f=fun x->
      match x.cmd with
      | CONNECTED->
        Lwt_log.info "recieved CONNECTED frame from server"
      | _->
        Lwt_log.info "expected a CONNECTED frame but got something else"
    in
    start_connection login pass ic oc >>= fun () ->
    print_endline "before protocol read_frame in client";
    lwt () = Lwt_log.info "before protocol read_Frame in client" in
    Protocol.read_frame ic >>= f>>=
    fun fr ->
    Lwt.async handle_connection;
    repl ()
  (*Lwt_log.info "completed both loops?"*)
  (* f >> repl () *)
  with
  | Failure _ ->
    return (ANSITerminal.(print_string [red]
                            "\n\nError. Malformed IP Address.\n"))
  | _ -> return (print_endline "Some other error")

let () = Lwt_log.add_rule "*" Lwt_log.Info

