open Client

module type Room =
  sig
    type t = username list
    (* Add the member with [username] to this room*)
    val add_member : username-> unit
    (* Remove the member with [username] from this room*)
    val rem_member: username-> unit
  end


module type Chatbot_room =
  sig
    include Room
    val create_msg  : string -> string
  end
