(*
 * test_protocol.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)

open OUnit2
open Protocol

let fr1 = { cmd = SEND; headers = ["destination","/queue/a";"content-length","14"]; body="hello queue a"}
let fr1_str = "SEND\ndestination:/queue/a\ncontent-length:14\n\nhello queue a\x00"
let contents = Buffer.contents

let tests = "test suite" >::: [
  "dummy" >:: (fun _ -> assert_equal 1 ( 1 ));
  "pack1" >:: (fun _ -> assert_equal ~printer:(fun x -> x) fr1_str (contents (pack fr1)));
]

let _ = run_test_tt_main tests
