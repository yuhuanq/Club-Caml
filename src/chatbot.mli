(*
 * chatbot.mli
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)

(* [init ()] initiates a new chatbot instance *)
val init : ?botid:string -> unit -> unit

(* [ask query] is a response string from the chatbot *)
val ask : string -> string

