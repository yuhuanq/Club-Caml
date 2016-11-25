(*
 * client.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>, Eric Wang
 *
 * Distributed under terms of the MIT license.
 *)
(*
 * client
 * client host port establishes a connection on the port port of the machine named
 * host, sends on the resulting socket the data it reads on its standard input and
 * writes the data it receives on its standard output.
 *)

open Sys
open Unix

(*
(* lib function Misc.retransmit fdin fdout reads data on the descriptor fdin *)
(* and writes it on fdout *)
let retransmit fdin fdout =
	let buffer_size = 4096 in
	let buffer = Bytes.create buffer_size in
  (* [ reads fd buff ofs len ] reads len bytes from descriptor fd, stores them *)
  (* in byte sequence buff, starting at position ofs in buff.
   * Returns number of bytes actually read *)
	let rec copy () = match read fdin buffer 0 buffer_size with
		 | 0 -> ()
     (* [write fd buff ofs len] writes len bytes to descriptor fd, taking them *)
     (* from byte sequence buff, starting at position ofs in buff. Returns number *)
     (* of bytes actually written *)
		 | n -> ignore (write fdout buffer 0 n); copy ()
	in copy ()

let server_name = Sys.argv.(1)
let port_number = int_of_string Sys.argv.(2)

let is_valid_args () =
  if Array.length Sys.argv < 3 then
    begin
      prerr_endline "Usage: client <host> <port>";
      exit 2
    end

let get_server_addr server_name =
  try
    (gethostbyname server_name).h_addr_list.(0)
  with Not_found ->
    prerr_endline (server_name ^ ": Host not found");
    exit 2

let client () =
  is_valid_args ();
  let server_addr = get_server_addr server_name in
  let sock = socket PF_INET SOCK_STREAM 0 in
  (* system call connect establishes a connection with a server on a socket. *)
  (* client side socket here *)
  connect sock (ADDR_INET(server_addr, port_number));
  (********************************
  *  A bit lost here on fork ()  *
  ********************************)
  (* val fork : unit -> int *)
  (* fork a new process. The returned integer is 0 for the child process, the pid *)
  (* of the child process for the parent process *)
  match fork () with
  | 0 ->
      (* stdin = file descriptor for standard input *)
      (* child process copies the data from its stdin to the socket *)
      retransmit stdin sock;
      (* close the socket for writing only.  *)
      shutdown sock SHUTDOWN_SEND;
      exit 0
  | _ ->
      (* read from sock and write to stdout *)
      (* parent process copies data it reads on the socket to its stdout *)
      retransmit sock stdout;
      (* closes the read and write sides of the connectoin, and deallocates the *)
      (* socket *)
      close stdout;
      wait ()
*)

let main ipstring = print_endline ("\nConnected to "^ipstring^"!\n")
(*
let _ = handle_unix_error client ()
*)
