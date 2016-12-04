(*gui_helper.ml has useful functions to maniuplate chat_buffer*)

let tag =
  let temp = GText.tag ~name:"msg_id_tag"() in
  temp#set_property (`WEIGHT (`BOLD));temp

(*the vertical scrollbar*)
let adjustment = GData.adjustment ()

let msg_insert (chat_buffer:GText.buffer) identifier msg =
  chat_buffer#insert ~iter:chat_buffer#end_iter ~tags:[tag]
                    identifier;
  chat_buffer#insert ~iter:chat_buffer#end_iter (msg^("\n"));
  adjustment#set_value (adjustment#upper) (*keep scrollbar at newest messages*)

let clear_chat (chat_buffer:GText.buffer) () =
  chat_buffer#delete (chat_buffer#get_iter `START) chat_buffer#end_iter;
  chat_buffer#insert "Welcome to Club Caml!\n"
