(*********************************************************
 gui_helper.ml has useful functions to manipulate the gui
 *********************************************************)

(*****These values are used for constructing the GUI*****)

(*[game_tag] is monospace, for better formatting of games*)
let game_tag =
  let temp = GText.tag ~name:"game_tag"() in
  temp#set_property (`FONT ("monospace"));
  temp#set_property (`WEIGHT (`BOLD));
  temp

let id_tag =
  let temp = GText.tag ~name:"msg_id_tag"() in
  temp#set_property (`WEIGHT (`BOLD));
  temp

let casted_id_tag = id_tag#as_tag
let casted_game_tag = game_tag#as_tag

let tag_table =
  let init_tag_table = GText.tag_table () in
  init_tag_table#add casted_id_tag;init_tag_table#add casted_game_tag;
  init_tag_table

let caml_pixbuf = GdkPixbuf.from_file_at_size "images/ocaml.png" 125 125
let clarkson_pixbuf = GdkPixbuf.from_file_at_size "images/clarkson.png" 125 125

let chat_buffer = GText.buffer ~tag_table:tag_table
                              ~text:"Welcome to Club Caml!\n" ()

(*the vertical scrollbar*)
let adjustment = GData.adjustment ()

(*See GObject.data_conv*)
let string_list_conv =
  let open Gobject in
  {kind=`STRING;
   proj=(fun x -> match x with
                  |`STRING y -> y
                  |_ -> failwith "conversion failed! not a string.");
   inj=(fun x -> match x with
                 |Some x -> `STRING (Some x)
                 |None -> `STRING (None) )
  }

let (user_list_store,column) = GTree.store_of_list string_list_conv []

let room_label = GMisc.label
            ~markup:"<span weight=\"bold\" size=\"larger\">Select a Room</span>"
            ~justify:`CENTER ()

(*----------------------------------------------------------------------------*)
(* [msg_insert identifier msg] inserts a message into the GUI.
 * The identifier should be of form "[9:52 PM] <Eric Wang>"
 * Takes in an optional boolean is_game that is used when
 * printing games (use monospace, bolded font for [msg]),
 * with default value false.*)
let msg_insert ?is_game:(game_bool=false) (identifier:string) (msg:string)  =
  if msg = "🐪" then   (*special caml case, get it? hahaha*)
    begin
      chat_buffer#insert ~iter:chat_buffer#end_iter ~tags:[id_tag]
                        ("\n"^identifier^"\n");
      chat_buffer#insert_pixbuf chat_buffer#end_iter caml_pixbuf
    end
  else if msg = "😶" then
    begin
      chat_buffer#insert ~iter:chat_buffer#end_iter ~tags:[id_tag]
                        ("\n"^identifier^"\n");
      chat_buffer#insert_pixbuf chat_buffer#end_iter clarkson_pixbuf
    end
  else if game_bool then
    begin
    chat_buffer#insert ~iter:chat_buffer#end_iter ~tags:[id_tag]
                      ("\n"^identifier^" ");
    chat_buffer#insert ~iter:chat_buffer#end_iter ~tags:[game_tag]
                       ("msg");
    end
  else
    begin
    chat_buffer#insert ~iter:chat_buffer#end_iter ~tags:[id_tag]
                      ("\n"^identifier^" ");
    chat_buffer#insert ~iter:chat_buffer#end_iter msg;
    end


(*[clear_chat ()] clears the GUI chat window*)
let clear_chat () =
  chat_buffer#delete (chat_buffer#get_iter `START) chat_buffer#end_iter;
  chat_buffer#insert "Welcome to Club Caml!\n"

(*[set_usr_list list] sets the user list in gui to list*)
let set_usr_list (user_list:string list) =
  user_list_store#clear ();
  let append_usr (usr:string) =
    let iter = user_list_store#append () in
    user_list_store#set iter column (Some usr)
  in
  List.iter append_usr user_list

(*[set_room_label room] sets the room label in the gui to [room]*)
let set_room_label (room:string) =
  room_label#set_label
    ("<span weight=\"bold\" size=\"larger\">"^room^"</span>")
