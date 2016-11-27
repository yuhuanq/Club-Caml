(*
 * server.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)

open Lwt
open Protocol

(* make server listen on 127.0.0.1:9000 *)
let listen_address = Unix.inet_addr_of_string ("162.243.63.41")
let port = 9000 (* or Sys.argv.(2) *)
let backlog = 10 (* max num of connections? not working *)

(* enable logging up to the INFO level *)
let () = Lwt_log.add_rule "*" Lwt_log.Info

let handle_message msg =
    msg

let rec handle_connection ic oc () =
    Lwt_io.read_line_opt ic >>=
    (fun msg ->
        match msg with
        | Some msg ->
            let reply = handle_message msg in
            Lwt_io.write_line oc reply >>= handle_connection ic oc
        | None -> Lwt_log.info "Connection closed" >>= return)

(* [greeting oc] is a message/prompt the server sends to a new connection  *)
let greeting oc =
  ignore (Lwt_io.write_line oc "Welcome.")

(*
 * [accept_connection conn] 'accepts' a connection from
 * [conn : descriptor * sockaddr] and creates a channel to the file descriptor,
 * sends a greeting, and calls [handle_connection]
 *)
let accept_connection conn =
    let fd, sckaddr = conn in
    let open Lwt_unix in
    (*--------------DEBUG------------------*)
    let _ = match sckaddr with
            |ADDR_INET (inet_addr,num) -> print_endline(Unix.string_of_inet_addr inet_addr)
            |_ -> () in
    (*--------------DEBUG------------------*)
    let client_id =
      match sckaddr with
      | ADDR_INET(inet_addr,num) ->
          let open Unix in
          string_of_inet_addr inet_addr
      | _ -> "unknown" in
    let ic = Lwt_io.of_fd Lwt_io.Input fd in
    let oc = Lwt_io.of_fd Lwt_io.Output fd in
    greeting oc;
    Lwt.on_failure (handle_connection ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
    Lwt_log.info ("New connection from " ^ client_id) >>= return

(*
 * [create_socket () ] creates a socket of type stream in the internet
 * domain with the default protocol and returns it
 *)
let create_socket () =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock @@ ADDR_INET(listen_address, port);
    listen sock backlog;
    sock

(*
 * [create_server] creates a socket with [create_socket] and enters an infinite
 * loop. At each iteration of the loop, it waits for a connection request with
 * accept and treats it with [accept_connection].
 *)
let create_server () =
  let server_socket = create_socket () in
  let rec serve () =
    let client = Lwt_unix.accept server_socket in
    client >>= accept_connection >>= serve
  in serve

(* initialize the server *)
let () =
    let serve = create_server () in
    Lwt_main.run @@ serve ()

let _ = {cmd = SEND ; headers = [] ; body = ""}
