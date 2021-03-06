(*
 * run_server.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)

let port = ref 9000
let verbose = ref false

let params = Arg.align [
  "--port", Arg.Set_int port, "Set the server to listen to this port (default: 8000)";
  "--verbose", Arg.Set verbose, "Write verbose debug info to stderr";
]

let usage_msg = "Usage: run_server [options]"

let () =
  Arg.parse
  params
  (fun s ->
    print_endline "Unknown argument";
    Arg.usage params usage_msg;
    exit 1;)
  usage_msg;
  Server.run_server !port !verbose

