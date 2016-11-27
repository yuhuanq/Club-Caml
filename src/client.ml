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
  let ()=print_endline "Enter login and password on separate lines" in
  let ()=print_string "username: " in
  let log=read_line () in
  let ()=print_string "password: " in
  let pass=read_line () in
  (log,pass)

let start_connection login pass servchannel=
  let conframe=make_connect login pass in
  let _=print_endline "line 7.1" in
  let newconn={input=Lwt_io.stdin;
              output=servchannel;
              topic=None;
              username=login} in
  let _=print_endline "line 7.2" in
  cur_connection:=newconn;
  let _=print_endline "line 7.3" in
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
  let _=print_endline "line 1" in
  try let inet_addr = inet_addr_of_string ipstring in
  let _=print_endline "line 2" in
  let foreignSockAddr = ADDR_INET (inet_addr,port) in
  let _=print_endline "line 3" in
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  let _=print_endline "line 4" in
  let () = Lwt_unix.bind sock (ADDR_INET (inet_addr_loopback,port)) in
  let _=print_endline "line 5" in
  let _ = Lwt_unix.connect sock foreignSockAddr in
  let _=print_endline "line 6" in
  let chToServer= Lwt_io.of_fd Lwt_io.output sock in
  let chFromServer=Lwt_io.of_fd Lwt_io.input sock in
  let _=print_endline "line 7" in
  let (login,pass)=read_password_and_login () in
  let _ =start_connection login pass chToServer
  in
  let f=function
        |x-> Lwt_io.print x.body
  in
  let _=(read_frame chFromServer >>=f)
  in
  let _=Lwt_io.print "something" in
  let _=print_endline "line 8" in

  ()



  with
  | Failure _ ->
          ANSITerminal.(print_string [red]
            "\n\nError. Malformed IP Address.\n")
  |_-> print_endline "Some other error"

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
