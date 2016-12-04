(*********************************************************
 gui_helper.ml has useful functions to manipulate the gui
 *********************************************************)

(*These values are used for construcing the GUI*)
let tag =
  let temp = GText.tag ~name:"msg_id_tag"() in
  temp#set_property (`WEIGHT (`BOLD));temp

let casted_tag = tag#as_tag

let tag_table =
  let init_tag_table = GText.tag_table () in
  init_tag_table#add casted_tag;init_tag_table

let chat_buffer = GText.buffer ~tag_table:tag_table
                              ~text:"Welcome to Club Caml!\n" ()

(*the vertical scrollbar*)
let adjustment = GData.adjustment ()

(*----------------------------------------------------------*)
(*[msg_insert identifier msg] inserts a message into the GUI.
 *The identifier should be of form "[9:52 PM] <Eric Wang>"*)
let msg_insert (identifier:string) (msg:string) =
  chat_buffer#insert ~iter:chat_buffer#end_iter ~tags:[tag]
                    identifier;
  chat_buffer#insert ~iter:chat_buffer#end_iter (msg^("\n"));
  adjustment#set_value (adjustment#upper) (*keep scrollbar at newest messages*)

(*[clear_chat ()] clears the GUI chat window*)
let clear_chat () =
  chat_buffer#delete (chat_buffer#get_iter `START) chat_buffer#end_iter;
  chat_buffer#insert "Welcome to Club Caml!\n"
