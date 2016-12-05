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

(*-----------------ENTRY MAIN LOOP-----------------*)
let main () = Lwt_main.run(
  ignore(GtkMain.Main.init ());
  Lwt_glib.install ();
  let waiter,wakener = Lwt.wait () in

  (****PROMPT USER FOR USERNAME, IP & PORT OF SERVER****)
  let welcome_prompt = GWindow.dialog ~title:"Welcome to Club Caml!"
                                    ~width:640 ~height:240 () in

  (*USERNAME STUFF*)
  let user_hbox = GPack.hbox ~packing:welcome_prompt#vbox#add () in
  let user_label = GMisc.label
            ~markup: ("<span weight=\"bold\" size=\"larger\">"
             ^"Username (max 9 characters)</span>") ()
  in
  user_hbox#add (user_label#coerce);
  let user_entry = GEdit.entry ~max_length:9
                               ~packing:user_hbox#add () in

  (*IP ADDRESS STUFF*)
  let ip_hbox = GPack.hbox ~packing:welcome_prompt#vbox#add () in
  let ip_label = GMisc.label
            ~markup: ("<span weight=\"bold\" size=\"larger\">"
             ^"IP Address of server</span>") ()
  in
  ip_hbox#add (ip_label#coerce);

  let ip_entry = GEdit.entry ~max_length:25
                             ~packing:ip_hbox#add () in

  (*PORT STUFF*)
  let port_hbox = GPack.hbox ~packing:welcome_prompt#vbox#add () in
  let port_label = GMisc.label
            ~markup: ("<span weight=\"bold\" size=\"larger\">"
             ^"Port of server (default 9000)</span>") ()
  in
  port_hbox#add (port_label#coerce);

  let port_entry = GEdit.entry ~max_length:5 ~text:"9000"
                             ~packing:port_hbox#add () in


  (*BUTTON STUFF*)
  let cancel_button = GButton.button ~label:"Cancel"
                                  ~packing:welcome_prompt#action_area#add () in

  let connect_button = GButton.button ~label:"Connect"
                                ~packing:welcome_prompt#action_area#add () in

  (*upon cancel button click, end program*)
  ignore(cancel_button#connect#clicked
    ~callback:(fun () -> ignore(Lwt.wakeup wakener ())));

  (*upon connect button click, connect and launch main window*)
  ignore(connect_button#connect#clicked
    ~callback:
     (fun () -> ignore(Client.main (ip_entry#text) (user_entry#text)
                                   (int_of_string port_entry#text));
                welcome_prompt#misc#hide ();
                Gui_main.main wakener ()));

  welcome_prompt#show ();

  waiter
  )

let () = main ()
