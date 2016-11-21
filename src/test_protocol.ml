(*
 * test_protocol.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)

open OUnit2
open Protocol

let fr1 = { cmd = SEND; headers = ["destination","/queue/a"]; body="hello queue a"}
let fr1_str =
"SEND
destination:/queue/a

hello queue a
^@"

let tests = "test suite" >::: [
  "dummy" >:: (fun _ -> assert_equal 1 ( 1 ));
  "pack1" >:: (fun _ -> assert_equal fr1_str ~printer:(fun x->x) (pack fr1));
  "unpack1" >:: (fun _ -> assert_equal fr1  (unpack fr1_str));
  "pack then unpack" >:: (fun _ -> assert_equal fr1 (unpack (pack fr1)));
]

let _ = run_test_tt_main tests

