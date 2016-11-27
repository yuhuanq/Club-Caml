(*
 * client.ml
 * Copyright (C) 2016 sb892 <sb892@cornell.edu>, Somrita Banerjee
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
  let conframe=make_connect login pass in
  let newconn={input=Lwt_io.stdin;
              output=servchannel;
              topic=None;
              username=login} in
  cur_connection:=newconn;
  send_frame conframe newconn.output

(*(* make server listen on 127.0.0.1:9000 *)
let listen_address = Unix.inet_addr_loopback (* or Sys.argv.(1) *)
let port = 9000 (* or Sys.argv.(2) *)
*)

(*let ipstring="162.243.63.41"*)
let port=9000
(* we're using the same port on the host machine and on the server*)
let backlog = 10

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
  let chToServer= Lwt_io.of_fd Lwt_io.output sock in
  let chFromServer= Lwt_io.of_fd Lwt_io.input sock in
  let (login,pass)=read_password_and_login () in
  let _ =start_connection login pass chToServer
  in
  let f=function
        |x-> Lwt_io.print x.body
  in
  let _ = (read_frame chFromServer >>=f)
  in
  let _=Lwt_io.print "something" in
  let _=print_endline "line 8" in
  ()
  with
  | Failure _ ->
          ANSITerminal.(print_string [red]
            "\n\nError. Malformed IP Address.\n")
  |_-> print_endline "Some other error"

let option_to_str s=
  match s with
  |Some x-> x
  |None -> ""

(*
(* [#change nrooom] changes room to nroom (unsubscribe and subscribe)
   [#leave] leaves room (unsubscribe)
   [#join nroom] joins a new room (requires not in any room currently)
   [#game] plays a game
   [#chatbot] changes to chatbot room
   [#quit] closes the connection to server
 Note: only change, leave, join, and quit implemented*)

let rec repl  =
  let directive=read_line () in
  let cur_topic=option_to_str ((!cur_connection).topic) in
  let firstletter=directive.[0] in
  match firstletter with
  |'#'->
    begin
    match directive with
    |"#leave"-> let unsubframe=make_unsubscribe cur_topic in
                send_frame unsubframe (!cur_connection).output
    |"#quit"-> let disconframe=make_disconnect in
               send_frame disconframe (!cur_connection).output
    |_->
        let partOfDir=String.sub directive 0 7 in
        begin
        match partOfDir with
        |"#change"->
          let nroom=String.sub directive 8 ((String.length directive)-8) in
          let unsubframe=make_unsubscribe cur_topic in
          let subframe=make_subscribe nroom in
          send_frame unsubframe (!cur_connection).output >>=
          (fun ()->send_frame subframe (!cur_connection).output)
        |_->
          let partOfDir2=String.sub directive 0 5 in
          begin
          match partOfDir2 with
          |"#join"->
          let nroom=String.sub directive 6 ((String.length directive)-6) in
          let subframe=make_subscribe nroom in
          send_frame subframe (!cur_connection).output
          |_-> failwith "Unimplemented"
          end
        end
    end
  | _->
    let msgid=string_of_float(Unix.gettimeofday ()) in
    let msgframe= make_message cur_topic directive msgid in
    send_frame msgframe (!cur_connection).output


(*Create a socket -> connect to the the server's address -> call Lwt_io.of_fd*)

(*
(*open a new channel*)
let make_a_new_channel=
  let client_channel=Lwt_io.make Lwt_io.output_channel in
  client_channel

(* close a channel*)
let close_channel=
  let messge=Lwt_io.close client_channel in
  Lwt_text.write stdout message

let close_connection=
  let ()=Protocol.make_disconnect in
  let ()=close_channel

let open_connection=
  let ochannel=make_a_new_channel in
  let (login,pass)=read_password_and_login in
  let ()=Protocol.make_connect login pass
  *)
  *)
