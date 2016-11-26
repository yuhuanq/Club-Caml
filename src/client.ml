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
open Lwt

let port_number = 9000
(*
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



let main ipstring =
  try let inet_addr = inet_addr_of_string ipstring in
  let foreignSockAddr = ADDR_INET (inet_addr,port_number) in
  let listenSock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  let () = Lwt_unix.bind listenSock (ADDR_INET (inet_addr_loopback,port_number)) in

  failwith "unimplemented"

  with
  | Failure _ ->
          ANSITerminal.(print_string [red]
            "\n\nError. Malformed IP Address.\n")








  (**************)
