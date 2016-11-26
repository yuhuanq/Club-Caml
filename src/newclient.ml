(*
 * client.ml
 * Copyright (C) 2016 sb892 <sb892@cornell.edu>, Somrita Banerjee
 *
 * Distributed under terms of the MIT license.
 *)

open Lwt
open Protocol

type connection = {
  input      : Lwt_io.input_channel;
  output     : Lwt_io.output_channel;
  (* Can only be subscribed to one topic or i.e. be in one chatroom at a time *)
  mutable topic      : string option;
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

let read_password_and_login n=
  let ()=print_endline "Enter login and password on separate lines" in
  let ()=print_string "username: " in
  let log=read_line () in
  let ()=print_string "password: " in
  let pass=read_line () in
  (log,pass)

let rec start_connection n=
  let (login,pass)=read_password_and_login () in
  let conframe=make_connect login pass in
  (* TODO: how do i make new channels? e.g. Lwt_io.make Lwt_io.input*)
  let newconn={input=Lwt_io.stdin;
              output=Lwt_io.stdout;
              topic=Some "";
              username=login} in
  cur_connection:=newconn;
  send_frame conframe newconn.output

(*(* make server listen on 127.0.0.1:9000 *)
let listen_address = Unix.inet_addr_loopback (* or Sys.argv.(1) *)
let port = 9000 (* or Sys.argv.(2) *)
*)

let server_address=162.243.63.41
let port=9000
let backlog = 10
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