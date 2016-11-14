(*
 * protocol.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)

(* Note: we might not implement transactions, i.e. begin/commit/abort *)
type client_cmd = SEND
                  | SUBSCRIBE
                  | UNSUBSCRIBE
                  | BEGIN
                  | COMMIT
                  | ABORT
                  | ACK
                  | DISCONNECT

type server_cmd = CONNECTED
                  | MESSAGE
                  | RECEIPT
                  | ERROR

type command = Client_cmd of client_cmd | Server_cmd of server_cmd

type frame = {
               cmd : command;
               headers : string * string list;
               body : string
             }

module ParseFrameStr = struct
  (*
   * TODO: specs
   *)
  let parse_headers buf =
    failwith "unimplemented"

  (*
   * TODO: specs
   *)
  let parse_body buf =
    failwith "unimplemented"

  (*
   * TODO: specs
   *)
  let parse_cmd buf =
    failwith "unimplemented"

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
  let frame_of_buf buf =
    failwith "unimplemented"
end


(*******************************************
*  functions to make string STOMP frames  *
*******************************************)
module MakeFrameStr = struct
  let mk_send dest msg =
    failwith "unimplemented"

  let mk_subscribe dest =
    failwith "unimplemented"

  let mk_unsubscribe dest =
    failwith "unimplemented"

  let mk_connect host =
    failwith "unimplemented"

  let mk_ack msgid subscription =
    failwith "unimplemented"

  let disconnect_frame = "DISCONNECT\n\n\x00\n"

  (********************
  *  might not impl  *
  ********************)
  let mk_begin = failwith "unimplemented"
  let mk_commit = failwith "unimplemented"
  let mk_abort = failwith "unimplemented"

  (*********************
  *  server commands  *
  *********************)
  let mk_connected = failwith "unimplemented"
  let mk_message = failwith "unimplemented"
  let mk_receipt = failwith "unimplemented"
  let mk_error = failwith "unimplemented"

  (*
   * server method.
   * [string_of_frame] is a formatted string in STOMP format. Given a frame record,
   * disect it and construct a STOMP formatted string.
   *)
  let string_of_frame frame =
    failwith "unimplemented"
end

