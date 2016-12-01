(*
 * run_client.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)

let port = ref 8000
let addr = ref "127.0.0.1"

let params = Arg.align [
  "--port", Arg.Set_int port, "Set the client to connect to this port (default: 8000)";
  "--addr", Arg.Set_string addr, "Set the client to connect to this address (default: localhost)";
]

let usage_msg = "Usage: camlclient [options]"

let entry () =
  ANSITerminal.(print_string [blue]
    "\n\nWelcome to Club Caml.\n");
  Lwt_main.run (Client.main !addr)

let () =
  Arg.parse
  params
  (fun s ->
    print_endline "Unknown argument";
    Arg.usage params usage_msg;
    exit 1;)
  usage_msg;
  entry ()

