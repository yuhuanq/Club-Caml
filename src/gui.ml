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


let locale = GtkMain.Main.init ()


(*--------------GTK objects used by other functions & main--------------*)
let tag =
  let temp = GText.tag ~name:"msg_id_tag"() in
  temp#set_property (`WEIGHT (`BOLD));temp

let castedTag = tag#as_tag

let tagTable =
  let initTagTable = GText.tag_table () in
  initTagTable#add castedTag;initTagTable

let chatBuffer = GText.buffer ~tag_table:tagTable
                              ~text:"Welcome to Club Caml!\n" ()
(*the vertical scrollbar*)
let adjustment = GData.adjustment ()

(*---------------------Useful functions-----------------------*)
(*[msgInsert identifier msg] inserts string message into the chat.
 *Format of identifier string is "10:11 PM] <Eric Wang>" *)
let msgInsert identifier msg =
  chatBuffer#insert ~iter:chatBuffer#end_iter ~tags:[tag]
                    identifier;
  chatBuffer#insert ~iter:chatBuffer#end_iter (msg^("\n"));
  adjustment#set_value (adjustment#upper)

let clearChat () =
  chatBuffer#delete (chatBuffer#get_iter `START) chatBuffer#end_iter



(*-----------------MAIN LOOP-----------------*)
let main () =
  let window = GWindow.window ~width:960 ~height:720 ~resizable:false
                              ~title:"Club Caml" () in

  let icon = GdkPixbuf.from_file "images/icon.png"  in
  window#set_icon (Some icon);

  let vbox = GPack.vbox ~packing:window#add () in
  ignore(window#connect#destroy ~callback:Main.quit);

  (*About Dialog*)
  let aboutDialog () =
    let authors = ["Yuhuan Qiu";"Eric Wang";"Somrita Banerjee";"Byungchan Lim"]
    in
    let license = "Distributed under terms of the MIT license." in
    let version = "Alpha 0.1" in
    let copyright = "2016" in
    let aboutPopup = GWindow.about_dialog ~authors:authors ~license:license
                        ~version:version ~copyright:copyright
                        ~name:"Club Caml" ()
    in
    ignore(aboutPopup#connect#response
      ~callback:(fun about ->aboutPopup#misc#hide ()));
    aboutPopup#show in

  (*IP Address add server dialog window - Event Handler*)
  let ipPrompt () =
    let ipAddrPrompt = GWindow.dialog ~title:"Enter IP Address of server"
                                      ~width:400 ~height:100 () in
    let ipEntry = GEdit.entry ~max_length:50
                              ~packing:ipAddrPrompt#vbox#add () in
    let cancelButton = GButton.button ~label:"Cancel"
                                    ~packing:ipAddrPrompt#action_area#add () in
    let okButton = GButton.button ~label:"Connect"
                                  ~packing:ipAddrPrompt#action_area#add () in
    ignore(cancelButton#connect#clicked
      ~callback:(fun () -> ipAddrPrompt#misc#hide ()));
    ignore(okButton#connect#clicked
      ~callback:
       (fun () -> print_endline(ipEntry#text);ipAddrPrompt#misc#hide ()));
    ipAddrPrompt#show in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let view_menu = factory#add_submenu "View" in


  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore(factory#add_item "Select Server" ~key:_I ~callback: (ipPrompt ()));
  ignore(factory#add_item "About" ~callback:(aboutDialog ()));
  ignore(factory#add_item "Quit" ~key:_Q ~callback: Main.quit);

  (*View menu*)
  let factory = new GMenu.factory view_menu ~accel_group in
  ignore(factory#add_item "Clear Chat" ~key:_R ~callback:(clearChat));

  (*Paned Window Widgets*)
  let masterPaned = GPack.paned `HORIZONTAL ~packing:vbox#add () in
  let leftPaned = GPack.paned `VERTICAL ~packing:masterPaned#add () in
  ignore(leftPaned#set_position (642));

  (*Chat box widget*)
  let scrolledWindow = GBin.scrolled_window ~vadjustment:adjustment
                                            ~packing:leftPaned#add () in

  let chatView = GText.view ~wrap_mode:`WORD ~editable:false
                            ~cursor_visible:false
                            ~packing:scrolledWindow#add () in

  ignore(chatView#set_buffer chatBuffer);


  (*
  (*Users in room*)
  let usrWindow = GBin.scrolled_window in
  let usrs = new GTree.column_list in
  let usrColumn = usrs#add Gobject.Data.string in
  let rightPaneUsrList = GTree.view ~model: *)

  let entryBox = GPack.hbox ~packing:leftPaned#add () in

  (*User text entry widget*)
  let enter_cb entry () =
    let text = entry#text in

    print_endline (text^("\n"));
    chatBuffer#insert ~iter:chatBuffer#end_iter ~tags:[tag]
                      "[10:32 PM] <Eric Wang> ";
    chatBuffer#insert ~iter:chatBuffer#end_iter (text^("\n"));
    adjustment#set_value (adjustment#upper); (*keep scrollbar at newest messages*)
    entry#set_text "" (*clear user text entry*)
  in

  let entry = GEdit.entry ~max_length:500 ~packing:entryBox#add () in
  ignore(entry#connect#activate ~callback:(enter_cb entry));
  entry#misc#set_size_request ~width:900 ~height:40 () ; (*make button width small*)


  (* Button *)
  let sendButton = GButton.button ~relief:`NONE
                              ~packing:entryBox#add () in
  ignore(sendButton#connect#clicked
    ~callback: (fun () -> (enter_cb entry ())));

  (*Add send image to button*)
  let _ = GMisc.image ~file:"images/send-button_tiny.png"
                              ~packing:sendButton#add () in
  (*start with focus on text entry box*)
  ignore(entry#misc#grab_focus ());

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()


let () = main ()
