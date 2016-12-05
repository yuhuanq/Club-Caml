(*
 * client.mli
 * Copyright (C) 2016
 * sb892 <sb892@cornell.edu> Somrita Banerjee,
 * yq56 <yq56@cornell.edu> Yuhuan Qiu,
 * ew366 <ew366@cornell.edu> Eric Wang,
 * bl458 <bl458@cornell.edu> Byungchan Lim
 *
 * Distributed under terms of the MIT license.
*)

type connection

type message

(* [handle_quit] handles a #quit directive *)
val handle_quit : unit -> unit Lwt.t

(* [process s] sends a SEND frame with body s to server  *)
val process : string -> unit Lwt.t

(* [main ipstring login port] starts the client *)
val main : string -> string -> int -> unit Lwt.t

