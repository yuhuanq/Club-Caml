(*
 * gui.ml
 * Copyright (C) 2016 ew366 <ew366@cornell.edu> Eric Wang
 * Distributed under terms of the MIT license.
 *)
(*GUI foundation based on https://ocaml.org/
 *learn/tutorials/introduction_to_gtk.html#Gtktutorial and
 *https://github.com/klartext/lablgtk2-ocaml-Tutorial*)

open GMain
open GdkKeysyms
open Gui_helper

(*--------------Function linking gui and client--------------*)

(*[enter_cb entry] takes in an entry widget and sends the cur. text to be
 *processed by Client.*)
let enter_cb entry () = (*This function should write to the output channel*)
  let text = entry#text in
  (* TODO: integrate with client here *)
  let open Lwt in
(*
 * How the line below works. Call Client.process which takes in the entry#text
 * and interprets it -> send a Frame to server -> receive Frame back -> writes
 * output using Gui_helper.msg_insert
 *)
  ignore_result (Client.process entry#text);

  print_endline ("[user entry] - "^text^("\n"));

  entry#set_text "" (*clear user text entry*)

(*-----------------MAIN LOOP-----------------*)
let main () = Lwt_main.run(
  ignore(GtkMain.Main.init ());
  Lwt_glib.install ();
  ignore(Client.main ("127.0.0.1"));
  let waiter,wakener = Lwt.wait () in
  let window = GWindow.window ~width:960 ~height:720 ~resizable:false
                              ~title:"Club Caml" () in

  let icon = GdkPixbuf.from_file "images/icon.png"  in
  window#set_icon (Some icon);

  let vbox = GPack.vbox ~packing:window#add () in
  ignore(window#connect#destroy (Lwt.wakeup wakener));

  (*About Dialog*)
  let about_dialog () =
    let authors = ["Yuhuan Qiu";"Eric Wang";"Somrita Banerjee";"Byungchan Lim"]
    in
    let license = License.license in
    let version = "Alpha 0.1" in
    let copyright = "2016" in
    let about_popup = GWindow.about_dialog ~authors:authors ~license:license
                        ~version:version ~copyright:copyright
                        ~name:"Club Caml" ()
    in
    ignore(about_popup#connect#response
      ~callback:(fun about -> about_popup#misc#hide ()));
    about_popup#show in

  (*IP Address add server dialog window - Event Handler*)
  let ipPrompt () =
    let ip_addr_prompt = GWindow.dialog ~title:"Enter IP Address of server"
                                      ~width:400 ~height:100 () in
    let ip_entry = GEdit.entry ~max_length:50
                              ~packing:ip_addr_prompt#vbox#add () in
    let cancel_button = GButton.button ~label:"Cancel"
                                    ~packing:ip_addr_prompt#action_area#add () in
    let ok_button = GButton.button ~label:"Connect"
                                  ~packing:ip_addr_prompt#action_area#add () in
    ignore(cancel_button#connect#clicked
      ~callback:(fun () -> ip_addr_prompt#misc#hide ()));
    ignore(ok_button#connect#clicked
      ~callback:
       (fun () -> print_endline(ip_entry#text);ip_addr_prompt#misc#hide ()));
    ip_addr_prompt#show in

  (* Menu bar *)
  let menu_bar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menu_bar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let view_menu = factory#add_submenu "View" in


  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore(factory#add_item "Select Server" ~key:_I ~callback: (ipPrompt ()));
  ignore(factory#add_item "About" ~callback:(about_dialog ()));
  ignore(factory#add_item "Quit" ~key:_Q ~callback: Main.quit);

  (*View menu*)
  let factory = new GMenu.factory view_menu ~accel_group in
  ignore(factory#add_item "Clear Chat" ~key:_R ~callback:(clear_chat));

 (*Label for current room*)
  vbox#add (room_label#coerce);

  (*must create chat_and_info pane first (isntead of below)
   *because label needs to use it*)
  let chat_and_info = GPack.paned `HORIZONTAL () in

  (*Menu option to hide label for current room*)
  let label_hidden = ref false in
  ignore(factory#add_item "Toggle Room Label" ~key:_L
       ~callback:
          (fun () -> if !label_hidden = false then
                       begin
                         (label_hidden:=true;room_label#misc#hide ());
                         chat_and_info#misc#set_size_request ~height:650 ()
                       end
                     else
                       begin
                         label_hidden:=false;
                         room_label#misc#show ();
                         chat_and_info#misc#set_size_request ~height:623 ()
                       end
          ));

  (*chat box and user info PANED*)
  vbox#add (chat_and_info#coerce);
  chat_and_info#misc#set_size_request ~height:623 ();
  chat_and_info#set_position 810;

  (*Chat box widget*)
  let scrolled_window = GBin.scrolled_window ~vadjustment:Gui_helper.adjustment
                                             ~packing:chat_and_info#add () in
  scrolled_window#set_hpolicy `NEVER;
  scrolled_window#set_vpolicy `AUTOMATIC;
  scrolled_window#misc#set_size_request ~height:650 ();

  let chat_view = GText.view ~wrap_mode:`CHAR ~editable:false
                             ~cursor_visible:false
                             ~packing:scrolled_window#add () in

  ignore(chat_view#set_buffer chat_buffer);
  (*upon chat buffer change, scroll to lowest possible place*)
  ignore(
    ignore(chat_buffer#connect#changed
      (fun () -> ignore(adjustment#set_value (adjustment#upper));

                 ignore(chat_view#scroll_to_iter (chat_buffer#end_iter));
                 ()
   )));

  (*Users in room stuff*)
  let scrolled_usr = GBin.scrolled_window ~packing:chat_and_info#add () in
  scrolled_usr#set_hpolicy `NEVER;
  scrolled_usr#set_vpolicy `AUTOMATIC;
  let usr_view = GTree.view ~model:user_list_store
                            ~packing:scrolled_usr#add () in

  let usr_view_column = GTree.view_column  ~title:"Users in Room"
            ~renderer:(GTree.cell_renderer_text [`XALIGN 0.5],
                       ["text",column]) ()
  in
  usr_view_column#set_alignment 0.5;
  ignore(usr_view#append_column usr_view_column);

  (*Menu option to hide users in room*)
  let usr_view_hidden = ref false in
  ignore(factory#add_item "Toggle User Panel" ~key:_H
       ~callback:(fun () -> if !usr_view_hidden = false then
                            (usr_view_hidden:=true;scrolled_usr#misc#hide ())
                            else (usr_view_hidden:=false;
                                  scrolled_usr#misc#show ())));


  (*---------------------------------------------------------*)
  let entry_box = GPack.hbox ~packing:vbox#add () in

  (*User text entry widget*)
  let entry = GEdit.entry ~max_length:500 ~packing:entry_box#add () in
  ignore(entry#connect#activate ~callback:(enter_cb entry));
  (*make button width small*)
  entry#misc#set_size_request ~width:920 ~height:40 () ;


  (* Send Button *)
  let send_button = GButton.button ~relief:`NORMAL
                                   ~packing:entry_box#add () in
  ignore(send_button#connect#clicked
    ~callback: (fun () -> (enter_cb entry ())));

  (*Add send image to button*)
  let _ = GMisc.image ~file:"images/send-button_tiny.png"
                              ~packing:send_button#add () in
  (*start with focus on text entry box*)
  ignore(entry#misc#grab_focus ());

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();

  waiter
  )

let () = main ()
