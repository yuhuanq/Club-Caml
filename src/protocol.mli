(*
 * protocol.mli
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)
open Lwt

type command = | SEND
               | SUBSCRIBE
               | UNSUBSCRIBE
               | BEGIN
               | COMMIT
               | ABORT
               | ACK
               | DISCONNECT
               | CONNECT
               | CONNECTED
               | MESSAGE
               | RECEIPT
               | ERROR
               | STATS
               | GAME
               | GAME_RESP

(* Stomp Frame representation type *)
type frame = {
  cmd     : command;
  headers : (string * string) list;
  body    : string
}

val str_of_cmd : command -> string

val cmd_of_str : string -> command

(*
 * [frame_of_buf buf] is a frame record. Given a string [buf] in STOMP format
 * construct a frame
 *
 * i.e.
 * [ frame_of_string "
 * SEND
 * destination:/queue/a
 * hello queue a
 * ^@" ]
 * is { cmd = SEND; headers = ["destination","/queue/a"]; body = "hello queue a" }
 *)
val unpack : string -> frame

(*
 * [pack frame] is a buffer representation of the STOMP frame.
 * [frame] is a frame record
 *)
val pack : frame -> Buffer.t

(* [send_frame buf oc] writes the buffer to output channel [oc] *)
val send_frame : frame -> Lwt_io.output_channel -> unit Lwt.t

(* [read_frame ic] is the resulting STOMP frame RECORD from reading from [ic]. *)
val read_frame : Lwt_io.input_channel -> frame Lwt.t

val get_header : frame -> string -> string

(* [make_disconnect] is a STOMP DISCONNECT frame *)
val make_disconnect  : frame

(* [make_send] is a STOMP SEND frame *)
val make_send        : string -> string -> frame

(* [make_subscribe] is a STOMP SUBSCRIBE frame *)
val make_subscribe   : string -> frame

(* [make_unsubscribe] is a STOMP UNSUBSCRIBE frame *)
val make_unsubscribe : string -> frame

(* [make_connect] is a STOMP CONNECT frame *)
val make_connect     : string -> string -> frame

(* [make_ack] is a STOMP ACK frame *)
val make_ack         : string -> frame

(* [make_connected] is a STOMP CONNECTED frame *)
val make_connected   : string -> frame

(* [make_message] is a STOMP MESSAGE frame *)
val make_message     : string -> string -> string -> string -> frame

(* [make_error] is a STOMP ERROR frame *)
val make_error       : string -> string -> frame

val make_stats  : (string * string) list -> frame

val make_game        : string -> string -> string -> string -> frame

val make_game_message   : string -> string * string -> string -> frame
