(*
 * test_protocol.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
*)

open OUnit2
open Protocol

let contents = Buffer.contents

let fr1 = { cmd = SEND; headers = ["destination","/queue/a";"content-length","14"]; body="hello queue a"}
let fr1_str = "SEND\ndestination:/queue/a\ncontent-length:14\n\nhello queue a\x00"

let connected = {cmd = CONNECTED; headers = ["session","233333"]; body=""}
let connd_str = "CONNECTED\nsession:233333\n\n\x00"

let fr2 = {cmd = ACK; headers=[]; body="" }
let fr2_str = "ACK\n\n\000"


let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_UNIX SOCK_RAW 0 in sock

let fd = create_socket ()
let ic = Lwt_io.of_fd Lwt_io.Input fd
let oc = Lwt_io.of_fd Lwt_io.Output fd

let tests = "test suite" >::: [
  "pack1" >:: (fun _ -> assert_equal ~printer:(fun x -> x) fr1_str (contents (pack fr1)));
  "pack2" >:: (fun _ -> assert_equal ~printer:(fun x -> x) connd_str (contents (pack connected)));
  "pack3" >:: (fun _ -> assert_equal ~printer:(fun x -> x) fr2_str (contents (pack fr2)));
]

let _ = run_test_tt_main tests
