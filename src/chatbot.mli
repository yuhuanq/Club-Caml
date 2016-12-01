(*
 * chatbot.mli
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)


val init : ?botid:string -> unit -> unit

val ask : string -> string

