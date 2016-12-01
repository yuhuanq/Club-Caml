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

let (>>) (dt : unit Lwt.t) (f : unit Lwt.t) = dt >>= (fun () -> f)

(*initialize client channel to an output that drops everything*)
(*type client_channel= output_channel ref
let cur_channel
let (client_channel:output_channel)= null
*)

let (emptyconn:connection)= {input=Lwt_io.zero; output=Lwt_io.null; topic=None; username=""}
let cur_connection= ref emptyconn

let update_topic top=
  (!cur_connection).topic<-Some top

let read_password_and_login ()=
  let ()= ANSITerminal.(print_string [cyan]
            "\nEnter login and password on seperate lines.\n") in
  let ()=print_string "username: " in
  let log=read_line () in
  let ()=print_string "password " in
  let ()= ANSITerminal.(print_string [red]
            "(WARNING:PLAIN TEXT)") in
  let ()=print_string ":" in
  let pass=read_line () in
  let ()=print_string "\n\n" in
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
let backlog = 10

let option_to_str s=
  match s with
  |Some x-> x
  |None -> ""

let handle_leave cur_topic=
  let unsubframe=make_unsubscribe cur_topic in
  let f=function
        |x->
          match x.cmd with
          |INFO-> Lwt_log.info ("INFO frame recvd")>>
          Lwt_log.info ("body of frame recvd: "^x.body)
          (* TODO: print header to user*)
          |_-> Lwt_log.info "expected INFO frame"
  in
  Protocol.send_frame unsubframe (!cur_connection).output >>
  (read_frame (!cur_connection).input >>= f)

let handle_quit () =
  print_endline "Quitting the application\n";
  let disconframe=make_disconnect in
  Protocol.send_frame disconframe (!cur_connection).output

let handle_change nroom cur_topic=
  let unsubframe=make_unsubscribe cur_topic in
  let subframe=make_subscribe nroom in
  let f=function
        |x->
          match x.cmd with
          |INFO-> Lwt_io.print ("INFO frame recvd")>>
          Lwt_io.print ("body of frame recvd: "^x.body)
          (* TODO: print header to user*)
          |_-> Lwt_io.print "expected INFO frame"
  in
  send_frame unsubframe (!cur_connection).output >>
  (read_frame (!cur_connection).input >>=f)>>
  send_frame subframe (!cur_connection).output


let handle_join nroom=
  print_endline ("Attempting to join room "^nroom^"\n");
  let subframe=make_subscribe nroom in
  update_topic nroom;
  send_frame subframe (!cur_connection).output

let handle_message msg cur_topic=
  let msgframe= make_send cur_topic msg in
  send_frame msgframe (!cur_connection).output

let handle_game_client_side game_msg cur_topic =
  let sender=(!cur_connection).username in
  let gameframe = Protocol.make_game cur_topic game_msg sender in
  send_frame gameframe (!cur_connection).output

(* TODO: handle incoming messages*)

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
  print_endline "in repl";
  let directive=read_line () in
  let cur_topic=option_to_str ((!cur_connection).topic) in
  let firstletter=directive.[0] in
  match firstletter with
  |'#'->
    begin
    match directive with
    |"#leave"-> handle_leave cur_topic
    |"#quit"->
        print_endline "matched #quit";
        handle_quit ()
    |_->
        let partOfDir=String.sub directive 0 7 in
        begin
        match partOfDir with
        |"#change"->
          let nroom=String.sub directive 8 ((String.length directive)-8) in
          handle_change nroom cur_topic
        |_->
          let partOfDir2=String.sub directive 0 5 in
          begin
          match partOfDir2 with
          |"#join"->
            let nroom=String.sub directive 6 ((String.length directive)-6) in
            print_endline ("joining " ^ nroom);
            handle_join nroom >> repl ()
          |"#game" ->
            let game_msg=String.sub directive 6 ((String.length directive)-6) in
            handle_game_client_side game_msg cur_topic
          | "#chatbot" -> failwith "Unimplemented"
          | _ -> failwith "invalid # command"
          end
        end
    end
  | _->
    handle_message directive cur_topic
  >>
  Lwt_log.info "Sent a frame"
  >> repl ()

(*
 * [main () ] creates a socket of type stream in the internet
 * domain with the default protocol and returns it
 *)

let main ipstring =
  (* try_lwt *)
    let inet_addr : Lwt_unix.inet_addr = Unix.inet_addr_of_string ipstring in
    let addr = Lwt_unix.ADDR_INET (inet_addr,port) in
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    (*Do not need to bind, it is implicitly done - google this*)
    print_endline "right before lwt_unix.connect sock addr";
    lwt () = Lwt_unix.connect sock addr in
    (* >>= fun () -> *)
    let oc = Lwt_io.of_fd Lwt_io.Output sock in
    let ic = Lwt_io.of_fd Lwt_io.Input sock in
    print_endline "right before read pw";
    let (login,pass) = read_password_and_login () in
    let f=fun x->
            match x.cmd with
            | CONNECTED->
                return (print_endline "CONNECTED frame rec")
            | _-> return (print_endline "expected CONNECTED frame")
    in
    start_connection login pass ic oc >>= fun () ->
    print_endline "before protocol read_frame in client";
    lwt () = Lwt_log.info "before protocol read_Frame in client" in
    Protocol.read_frame ic >>= f>>= fun fr ->
    repl ()
    (* f >> repl () *)
  (*
   * with
   * | Failure _ ->
   *         return (ANSITerminal.(print_string [red]
   *           "\n\nError. Malformed IP Address.\n"))
   * | _ -> return (print_endline "Some other error")
   *)

let () = Lwt_log.add_rule "*" Lwt_log.Info
