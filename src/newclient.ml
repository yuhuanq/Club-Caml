open Lwt
open Protocol

(*initialize client channel to an output that drops everything*)
let (client_channel:output_channel)= null

let read_password_and_login=
  failwith "unimplemented"

let make_a_new_channel=
  let client_channel=Lwt_io.make Lwt_io.output_channel in
  client_channel

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