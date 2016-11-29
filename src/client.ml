(*
 * client.ml
 * Copyright (C) 2016 sb892 <sb892@cornell.edu> Somrita Banerjee,
                      ew366 <ew366@cornell.edu> Eric Wang
 *
 * Distributed under terms of the MIT license.
 *)

open Unix
open Lwt
open Protocol

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

let start_connection login pass servchannel=
  let conframe=Protocol.make_connect login pass in
  let newconn={input=Lwt_io.stdin;
              output=servchannel;
              topic=None;
              username=login} in
  cur_connection:=newconn;
  Protocol.send_frame conframe newconn.output

(*(* make server listen on 127.0.0.1:9000 *)
let listen_address = Unix.inet_addr_loopback (* or Sys.argv.(1) *)
let port = 9000 (* or Sys.argv.(2) *)
*)

(*let ipstring="162.243.63.41"*)
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
  send_frame unsubframe (!cur_connection).output>>
  (read_frame (!cur_connection).input >>=f)

let handle_quit =
  let _=print_endline "Quitting the application\n" in
  let disconframe=make_disconnect in
  send_frame disconframe (!cur_connection).output

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
  let _=print_endline ("Attempting to join room "^nroom^"\n") in
  let subframe=make_subscribe nroom in
  send_frame subframe (!cur_connection).output

let handle_message msg cur_topic=
  let msgid=string_of_float(Unix.gettimeofday ()) in
  let sender=(!cur_connection).username in
  let msgframe= make_message cur_topic msgid sender msg in
  send_frame msgframe (!cur_connection).output

(* TODO: handle incoming messages*)

(* [#change nrooom] changes room to nroom (unsubscribe and subscribe)
   [#leave] leaves room (unsubscribe)
   [#join nroom] joins a new room (requires not in any room currently)
   [#game] plays a game
   [#chatbot] changes to chatbot room
   [#quit] closes the connection to server
 Note: only change, leave, join, and quit implemented*)

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
    |"#quit"-> handle_quit
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
            handle_join nroom
          |_-> failwith "Unimplemented"
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
  let open Lwt_unix in
  try let inet_addr = inet_addr_of_string ipstring in
  let foreignSockAddr = ADDR_INET (inet_addr,port) in
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  (*Do not need to bind, it is implicitly done - google this*)
  (*let () = Lwt_unix.bind sock (ADDR_INET (inet_addr_loopback,port)) in*)
  let _ = Lwt_unix.connect sock foreignSockAddr in
  let oc= Lwt_io.of_fd Lwt_io.Output sock in
  let ic= Lwt_io.of_fd Lwt_io.Input sock in
  let (login,pass)=read_password_and_login () in
  let f=function
        |x->
          match x.cmd with
          |CONNECTED->
              return (print_endline "CONNECTED frame rec")
              (* Lwt_io.print ("CONNECTED frame recvd")>>= *)
          (* (fun ()->Lwt_io.print ("body of frame recvd: "^x.body)) *)
          |_-> Lwt_io.print "expected CONNECTED frame" in
  start_connection login pass oc >>=
  (fun () -> print_endline "before protocol read_frame in client";
  Protocol.read_frame ic)
  >>= f >> repl ()


  (* let _ = *)
  (* (start_connection login pass chToServer *)
    (* >>=(fun ()->(read_frame chFromServer >>=f))) *)

  (* >> Lwt_io.print "printing something\n" *)
  (* >> repl () *)
  (* in () *)
  with
  | Failure _ ->
          return (ANSITerminal.(print_string [red]
            "\n\nError. Malformed IP Address.\n"))
  |_-> return (print_endline "Some other error")



