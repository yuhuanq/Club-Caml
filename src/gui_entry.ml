(*
 * gui_entry.ml
 * Copyright (C) 2016 ew366 <ew366@cornell.edu> Eric Wang
 * Distributed under terms of the MIT license.
 *)

(*[Gui_entry] is the entry point for the client. The user is prompted here for
 *their username, ip address of server, and port in a dialog. Then
 *[Client] is called (does the communication between client/server) and
 *then [Gui_main], which displays the main client GUI*)

open GMain
open GdkKeysyms
open Gui_helper

type usr_error = Ip_error|Port_error

(*-------User Entry validator functions------*)
let is_valid_ip_addr (ip_string:string) =
  try
    ignore(Unix.inet_addr_of_string ip_string);
    true
  with
  | Failure _ -> false

let is_valid_port (port:string) =
  try
    let port = int_of_string port in
    (port >= 1024 && port <= 49151)
  with
  |Failure _ -> false

(*[process usr] checks if [usr] is empty. if so, returns an anonymous user
 *string (e.g. anon1917). else, return [usr] *)
let process_usr (usr:string) =
  if (String.length usr) = 0 then
    begin
    Random.self_init ();
    let rand_int = Random.int 10000 in
    "anon"^(string_of_int rand_int)
    end
  else
    usr

(*-----------------ENTRY MAIN LOOP-----------------*)
let main () = Lwt_main.run(
  ignore(GtkMain.Main.init ());
  Lwt_glib.install ();
  let waiter,wakener = Lwt.wait () in

  (****PROMPT USER FOR USERNAME, IP & PORT OF SERVER****)
  let welcome_prompt = GWindow.dialog
                ~title:"Welcome to Club Caml!"
                ~width:640 ~height:240 () in

  (*USERNAME STUFF*)
  let user_hbox = GPack.hbox ~packing:welcome_prompt#vbox#add () in
  let user_label = GMisc.label
        ~markup: ("<span weight=\"bold\" size=\"larger\">Username</span>"
         ^" (max 9 characters) \n"
         ^"<small>Leave blank to stay anonymous.</small>") ()
  in
  user_hbox#add (user_label#coerce);
  let user_entry = GEdit.entry ~max_length:9
                               ~packing:user_hbox#add () in

  (*IP ADDRESS STUFF*)
  let ip_hbox = GPack.hbox ~packing:welcome_prompt#vbox#add () in
  let ip_label = GMisc.label
            ~markup: ("<span weight=\"bold\" size=\"larger\">"
             ^"IP Address of server</span>            ") ()
  in
  ip_hbox#add (ip_label#coerce);
  ip_label#misc#set_size_request ~width:285 () ;
  let ip_entry = GEdit.entry ~max_length:25 ~text:"127.0.0.1"
                             ~packing:ip_hbox#add () in

  (*PORT STUFF*)
  let port_hbox = GPack.hbox ~packing:welcome_prompt#vbox#add () in
  let port_label = GMisc.label
            ~markup: ("<span weight=\"bold\" size=\"larger\">"
             ^"Server Port</span>                                \n"
             ^"<small>Server default is 9000.</small>") ()
  in
  port_hbox#add (port_label#coerce);
  port_label#misc#set_size_request ~width:285 () ;

  let port_entry = GEdit.entry ~max_length:5 ~text:"9000"
                             ~packing:port_hbox#add () in

  (*Error dialog*)
  let create_error_dialog (error:usr_error) =
    let error_message =
      match error with
      |Ip_error -> "IP address is malformed! Try again."
      |Port_error -> "Invalid port (must be between 1024 and 49151 inclusive)."
    in
    let error_dialog = GWindow.message_dialog ~message:error_message
                                              ~message_type:`ERROR
                                              ~buttons:GWindow.Buttons.ok ()
    in
    ignore(error_dialog#connect#response
      ~callback:(fun _ -> error_dialog#misc#hide ()));
    error_dialog#misc#show ()
  in

  (*BUTTON STUFF*)
  let cancel_button = GButton.button ~label:"Cancel"
                                  ~packing:welcome_prompt#action_area#add () in

  let connect_button = GButton.button ~label:"Connect"
                                ~packing:welcome_prompt#action_area#add () in

  (*-----------Client launch function---------*)

  (*[launch_main ()] launches Client and Gui_main with the user-specified
   *parameters. SHOULD ONLY BE CALLED AFTER USER INPUT VALIDATION!*)
  let launch_main ip user port =
    ignore(Client.main (ip) (user)
                       (int_of_string port));
    welcome_prompt#misc#hide ();
    Gui_main.main wakener ()
  in

  (*----------Button callback registrations-------*)
  (*upon cancel button click, end program*)
  ignore(cancel_button#connect#clicked
    ~callback:(fun () -> ignore(Lwt.wakeup wakener ())));

  (*upon connect button click, validate entries,
   *then connect and launch main window*)
  ignore(connect_button#connect#clicked
    ~callback:
     (fun () -> let ip_str = if ip_entry#text = "localhost" then
                               "127.0.0.1"
                             else
                               ip_entry#text
                in
                let usr_str = process_usr user_entry#text in
                let port_str = port_entry#text in
                if (is_valid_ip_addr ip_str) then
                  if (is_valid_port port_str) then
                    launch_main ip_str usr_str port_str
                  else
                    create_error_dialog Port_error
                else
                  create_error_dialog Ip_error));

  welcome_prompt#show ();

  waiter
  )

let () = main ()
