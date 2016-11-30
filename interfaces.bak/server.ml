(*
 * server.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)

open Lwt


(* make server listen on 127.0.0.1:9000 *)
(* let listen_address = Sys.argv.(1) *)
let listen_address = Unix.inet_addr_loopback
(* let port = Sys.argv.(2) *)
let port = 9000
let backlog = 10

(* enable logging up to the INFO level *)
let () = Lwt_log.add_rule "*" Lwt_log.Info

let handle_message msg =
    msg
    (* match msg with *)
    (* | "read" -> string_of_int !counter *)
    (* | "inc"  -> counter := !counter + 1; "Counter has been incremented" *)
    (* | _      -> "Unknown command" *)


let rec handle_connection ic oc () =
    Lwt_io.read_line_opt ic >>=
    (fun msg ->
        match msg with
        | Some msg ->
            let reply = handle_message msg in
            Lwt_io.write_line oc reply >>= handle_connection ic oc
        | None -> Lwt_log.info "Connection closed" >>= return)

let accept_connection conn =
    let fd, _ = conn in
    let ic = Lwt_io.of_fd Lwt_io.Input fd in
    let oc = Lwt_io.of_fd Lwt_io.Output fd in
    Lwt.on_failure (handle_connection ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
    Lwt_log.info "New connection" >>= return

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
let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve


let () =
    let sock = create_socket () in
    let serve = create_server sock in
    Lwt_main.run @@ serve ()

(*
 * [tcp_server] creates a socket with install_tcp_server and enters an infinite
 * loop. At each iteration of the loop it waits for a connection request with
 * accept and treats it with the function treat_connection. Restart the accept
 * call if it is interrupted. We also ignore the signal sigpipe so that unexpected
 * disconnection raise an EPIPE exception that can be caught by treat_connection
 * rather than killing the server.
 *)
(*
 * let tcp_server treat_connection addr =
 *   ignore (signal sigpipe Signal_ignore);
 *   let server_sock = install_tcp_server_socket addr in
 *   while true do
 *       let client = restart_on_EINTR accept server_sock in
 *       treat_connection server_sock client
 *   done
 *)











