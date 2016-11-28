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

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore(factory#add_item "Select Server" ~key:_I ~callback: Main.quit);
  ignore(factory#add_item "Quit" ~key:_Q ~callback: Main.quit);

  (*
  (* Button *)
  let button = GButton.button ~label:"Push me!"
                              ~packing:vbox#add () in
  ignore(button#connect#clicked ~callback: (fun () -> print_endline "Ouch!"));
  *)

  (*Paned Window Widgets*)
  let paned = GPack.paned `VERTICAL ~packing:vbox#add () in
  ignore(paned#set_position (600));

  (*Chat box widget*)
  let view = GText.view ~wrap_mode:`WORD ~editable:false ~packing:paned#add () in
  ignore(view#connect);

  (*User text entry widget*)
  let enter_cb entry () =
    let text = entry#text in

    print_endline (text^("\n"));
    entry#set_text "" in

  let entry = GEdit.entry ~max_length:500 ~packing:paned#add () in
  ignore(entry#connect#activate ~callback:(enter_cb entry));



  (*start with focus on text entry box*)
  ignore(entry#misc#grab_focus ());


  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()
