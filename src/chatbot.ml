(*
 * chatbot.ml
 * Copyright (C) 2016 yqiu <yqiu@f24-suntzu>
 *
 * Distributed under terms of the MIT license.
 *)

(*
 * API wrapper for pandorabots
 *
 * Example usage:
 *   # Chatbot.init ()
 *   # Chatbot.ask "Hello!"
 *      I am good. How are you?
 *
 * raises Unsuccessful if HTTP request status is non 200
 * raises Failedreq if nonzero code in response frame
 *)


open Lwt
open Cohttp
open Cohttp_lwt_unix

let (>>|) = (>|=)
let (>>) (dt : unit Lwt.t) f = dt >>= (fun _ -> f)

let host = "www.pandorabots.com"
let protocol = "http://"
let resource = "/pandora/talk-xml"
let api_url = protocol ^ host ^ resource

let botid = ref "ea77c0200e365cfb"  (* default einstein bot *)
let custid_ref = ref None

let fst_query = ref true

exception Unsuccessful of string (* non 200 code *)
exception Failedreq of string (* nonzero code in response frame *)
exception Failed_to_find_bot

let send_query s (id : string option) =
  Printf.printf "Making request to: %s\n" api_url;
  match id with
  | None ->
      let params = ["botid",[!botid];"input",[s]] in
      Cohttp_lwt_unix.Client.post_form params (Uri.of_string api_url) >>= fun (resp,body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Printf.printf "Response code: %d\n" code;
      Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
      body |> Cohttp_lwt_body.to_string >>= fun body ->
      Printf.printf "Length of body: %d\n" (String.length body);
      if code=200 then return body
      else fail (Unsuccessful body)
      (* body *)
  | Some cid ->
      let params = ["botid",[!botid];"input",[s];"custid",[cid]] in
      Cohttp_lwt_unix.Client.post_form params (Uri.of_string api_url) >>= fun (resp,body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Printf.printf "Response code: %d\n" code;
      Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
      body |> Cohttp_lwt_body.to_string >>= fun body ->
      Printf.printf "Length of body: %d\n" (String.length body);
      if code=200 then return body
      else fail (Unsuccessful body)
      (* body *)


let rec extract_pcdata tagt = function
  | [] -> ""
  | h::t ->
    if Xml.tag h = tagt then
      let msg = List.fold_left
      (fun acc x -> match x with Xml.PCData s -> s ^ acc | _ -> acc)
      "" (Xml.children h) in
      msg
    else extract_pcdata tagt t

(*
 * <result status="3" botid="ea77c0200e365cfb 0291fdfd">
 *   <input>Whats up mate</input>
 *   <message>Failed to find bot</message>
 * </result>
 *)
let parse_error resp =
  let x = Xml.parse_string resp in
  let children = Xml.children x in
  (extract_pcdata "message" children)

(*
 * Example xml:
 * <result status="0" botid="ea77c0200e365cfb" custid="abcec6d1fe35faf2">
 *   <input>Whats up mate</input>
 *   <that>Many interesting things, I'm sure.  But, why don't you tell me what's up with you?</that>
 * </result>
 *)
let parse resp =
  let x = Xml.parse_string resp in
  let status = Xml.attrib x "status" in
  if (int_of_string status) != 0 then
    raise (Failedreq (parse_error resp))
  else
    let custid = Xml.attrib x "custid" in
    (extract_pcdata "that" (Xml.children x)),custid

let ask' query =
  if !fst_query then
    (* it is the first query *)
    send_query query None >>= fun body ->
    print_endline "in try_lwt after ask";
    print_endline ("Received body\n" ^ body);
    let resptxt,custid = parse body in
    fst_query := false;
    custid_ref := Some custid;
    (* Lwt_io.print resptxt >> *)
    return resptxt
  else
    send_query query !custid_ref >>= fun body ->
    print_endline "in try_lwt after ask";
    print_endline ("Received body\n" ^ body);
    let resptxt,custid = parse body in
    (* Lwt_io.print resptxt >> *)
    return resptxt

let ask query =
  Lwt_main.run (ask' query)

let init ?botid:(bid="ea77c0200e365cfb") () =
  (* init a new pandorabot *)
  fst_query := true;
  botid := bid;
  custid_ref := None

(*
 * let () =
 *   init ();
 *   print_endline (ask "whats up");
 *   print_endline (ask "anything")
 *)

