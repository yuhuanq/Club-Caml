(*
 * gui.ml
 * Copyright (C) 2016 ew366 <ew366@cornell.edu> Eric Wang
 * Distributed under terms of the MIT license.
 *)
(*GUI based on https://ocaml.org/
 *learn/tutorials/introduction_to_gtk.html#Gtktutorial
 *https://github.com/klartext/lablgtk2-ocaml-Tutorial*)

open GMain
open GdkKeysyms

(*ocamlbuild -pkgs lablgtk2 gui.byte*)

let locale = GtkMain.Main.init ()

let main () =
  let window = GWindow.window ~width:960 ~height:720
                              ~title:"Club Caml" () in
  let vbox = GPack.vbox ~packing:window#add () in
  ignore(window#connect#destroy ~callback:Main.quit);


  (*About Dialog*)
  let aboutDialog () =
    let authors = ["Yuhuan Qiu";"Eric Wang";"Somrita Banerjee";"Byungchan Lim"] in
    let license = "Distributed under terms of the MIT license." in
    let version = "Alpha 0.1" in
    let copyright = "2016" in
    let aboutPopup = GWindow.about_dialog ~authors:authors ~license:license
                        ~version:version ~copyright:copyright
                        ~name:"Club Caml" ()
    in
    (*aboutPopup#connect#(aboutPopup.close response) ~callback:( ->aboutPopup#misc#hide);*)
    aboutPopup#show in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let _ = factory#add_item "About" ~callback:(aboutDialog ()) in

  (*IP Address add server dialog window - Event Handler*)
  let ipPrompt () =
    let ipAddrPrompt = GWindow.dialog ~title:"Enter IP Address of server"
                                      ~width:400 ~height:100 () in
    let ipEntry = GEdit.entry ~max_length:50
                              ~packing:ipAddrPrompt#vbox#add () in
    let okButton = GButton.button ~label:"Connect"
                                  ~packing:ipAddrPrompt#action_area#add () in
    ignore(okButton#connect#clicked
      ~callback: (fun () -> print_endline(ipEntry#text);ipAddrPrompt#destroy ()));
    ipAddrPrompt#show in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore(factory#add_item "Select Server" ~key:_I ~callback: (ipPrompt ()));
  ignore(factory#add_item "Quit" ~key:_Q ~callback: Main.quit);

  (*
  (* Button *)
  let button = GButton.button ~label:"Push me!"
                              ~packing:vbox#add () in
  ignore(button#connect#clicked ~callback: (fun () -> print_endline "Ouch!"));
  *)

  (*Paned Window Widgets*)
  let masterPaned = GPack.paned `HORIZONTAL ~packing:vbox#add () in
  let leftPaned = GPack.paned `VERTICAL ~packing:masterPaned#add () in
  ignore(leftPaned#set_position (600));

  (*Chat box widget*)
  let chatView = GText.view ~wrap_mode:`WORD ~editable:false
                            ~packing:leftPaned#add () in
  ignore(chatView#connect);

  (*Users in room*)
  (*let pane = failwith "unimplemented" in *)

  (*User text entry widget*)
  let enter_cb entry () =
    let text = entry#text in

    print_endline (text^("\n"));
    entry#set_text "" in

  let entry = GEdit.entry ~max_length:500 ~packing:leftPaned#add () in
  ignore(entry#connect#activate ~callback:(enter_cb entry));



  (*start with focus on text entry box*)
  ignore(entry#misc#grab_focus ());


  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()
