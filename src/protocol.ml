(*
 * protocol.ml
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
               | INFO

type frame = {
  cmd     : command;
  headers : (string * string) list;
  body    : string
}

let str_of_cmd = function
  | SEND        -> "SEND"
  | SUBSCRIBE   -> "SUBSCRIBE"
  | UNSUBSCRIBE -> "UNSUBSCRIBE"
  | BEGIN       -> "BEGIN"
  | COMMIT      -> "COMMIT"
  | ABORT       -> "ABORT"
  | ACK         -> "ACK"
  | DISCONNECT  -> "DISCONNECT"
  | CONNECT     -> "CONNECT"
  | CONNECTED   -> "CONNECTED"
  | MESSAGE     -> "MESSAGE"
  | RECEIPT     -> "RECEIPT"
  | ERROR       -> "ERROR"
  | INFO        -> "INFO"

let cmd_of_str = function
  | "SEND"        -> SEND
  | "SUBSCRIBE"   -> SUBSCRIBE
  | "UNSUBSCRIBE" -> UNSUBSCRIBE
  | "BEGIN"       -> BEGIN
  | "COMMIT"      -> COMMIT
  | "ABORT"       -> ABORT
  | "ACK"         -> ACK
  | "CONNECT"     -> CONNECT
  | "DISCONNECT"  -> DISCONNECT
  | "CONNECTED"   -> CONNECTED
  | "MESSAGE"     -> MESSAGE
  | "RECEIPT"     -> RECEIPT
  | "ERROR"       -> ERROR
  | _             -> failwith "illegal cmd string"

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
(* val unpack : string -> frame *)
let unpack buf =
  let sep = Str.regexp "\n" in
  let lst = Str.split sep buf in
  let lst = List.fold_right (fun elt acc -> if elt = "" then acc else elt::acc)
              lst [] in
  let cmd =
    try List.hd lst
    with _ -> failwith "empty str/non STOMP str fed to [unpack buf]" in
  let rec helper frame lst =
    let rec inner_helper frame lst =
      begin match lst with
      | [] -> frame
      | h::t -> if String.contains h ':' then
          let sep = Str.regexp ":" in
          let kv = Str.split sep h in
          inner_helper {frame with headers = frame.headers @ [List.nth kv 0,
                                                              List.nth kv 1]} t
        else {frame with body = h}
      end in
    match lst with
    | h::t -> inner_helper frame t
    | _ -> failwith "empty str/non STOMP str fed to [unpack buf]" in
  let placeholder_frame = {cmd = cmd_of_str cmd ; headers = []; body = "" } in
  helper placeholder_frame lst

(*
 * [have_cl frame] is true iff frame.cmd := SEND | MESSAGE | ERROR
 * cl stands for content-length header
 *
 * all other frames must not have a body
*)
let have_cl frame =
  match frame.cmd with
  | SEND | MESSAGE | ERROR -> true
  | _ -> false


(*
 * [pack frame] is a buffer representation of the STOMP frame.
 * [frame] is a frame record
*)
(* val pack : frame -> Buffer.t *)
let pack frame =
  let buf = Buffer.create 80 in
  Buffer.add_string buf (str_of_cmd frame.cmd);
  Buffer.add_char buf '\n';
  (* have content-length header last so that once read; can immediately read [len] *)
  (* bytes for the body *)
  List.iter (fun (k,v) ->
    if k <> "content-length" then
      begin
        Buffer.add_string buf k;
        Buffer.add_string buf ":";
        Buffer.add_string buf v;
        Buffer.add_char buf '\n';
      end
    else ()
  ) frame.headers;
  if have_cl frame then
    begin
      Buffer.add_string buf "content-length";
      Buffer.add_char buf ':';
      let len = (List.assoc "content-length" frame.headers) in
      Buffer.add_string buf (len);
      Buffer.add_char buf '\n';
    end
  else ();
  Buffer.add_char buf '\n';
  Buffer.add_string buf frame.body;
  Buffer.add_char buf '\x00';
  buf

let send_frame frame oc =
  let buf = pack frame in
  let payload = Buffer.contents buf in
  let _ = Lwt_io.write oc payload in
  Lwt_io.flush oc

(*
 * [read_to_null ic] reads from input_channel [ic] until the nullbyte \x00
 *
 * val read_to_null ic : Lwt_io.input_channel -> unit Lwt.t
*)
let rec read_to_null ic =
  let f = function
    | "\x00" -> return ()
    | _ -> read_to_null ic in
  Lwt_io.read_line ic >>= f

let read_frame ic =
  let cmd = Lwt_io.read_line ic in
  let rec read_headers acc =
    Lwt_io.read_line ic  >>=
    (fun s ->
       match s with
       (* headers done if empty str *)
       | "" -> return acc
       | s ->
         let sep = Str.regexp ":" in
         match Str.split sep s with
         | []    -> read_headers acc
         | [k;v] -> read_headers ((k,v)::acc)
         | h::t -> read_headers acc) in
  let final = (fun c ->
    read_headers [] >>=
    (fun lst ->
       try
         let read_len = List.assoc "content-length" lst in
         let read_len = int_of_string read_len in
         let bytebuf = Bytes.create read_len in
         let _ = Lwt_io.read_into_exactly ic bytebuf read_len 0 in
         return {cmd = cmd_of_str c; headers = lst ; body = bytebuf }
       with Not_found ->
          (*
          * if no content-length header then body is empty
          * and read to the nullbyte
         *)
         let _ = read_to_null ic in
         return {cmd = cmd_of_str c; headers = lst ; body = ""})) in
  cmd >>= final

(* [get_header frame name]  *)
let get_header frame name =
  (* try *)
  List.assoc name frame.headers
(* with Not_found -> *)
(* "" *)

let make_disconnect =
  {cmd    = DISCONNECT;
   headers = [];
   body    = "" }

let make_send dest msg =
  {cmd     = SEND;
   headers = ["destination",dest;
              "content-length",string_of_int (String.length msg)];
   body    = msg }

let make_subscribe dest =
  {cmd     = SUBSCRIBE;
   headers = ["destination",dest];
   body    = "" }

let make_unsubscribe dest =
  {cmd     = UNSUBSCRIBE;
   headers = ["destination",dest];
   body    = "" }

let make_connect login pass =
  {cmd     = CONNECT;
   headers = ["login",login;"passcode",pass];
   body    = "" }

let make_ack mid =
  {cmd     = ACK;
   headers = ["message-id",mid];
   body    = "" }

let make_connected sid =
  {cmd     = CONNECTED;
   headers = ["session",sid];
   body    = "" }

let make_message dest mid sender msg=
  {cmd     = MESSAGE;
   headers = ["destination",dest;
              "sender",sender;
              "message-id",mid;
              "content-length",string_of_int (String.length msg)];
   body    = msg }

let make_error message body =
  {cmd     = ERROR;
   headers = ["message",message;
              "content-length",string_of_int (String.length body)];
   body    = body }

let make_info headers =
  { cmd     = INFO;
    headers = headers;
    body    = ""}

