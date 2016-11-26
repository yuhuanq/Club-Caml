(* game_state is the current state of the game. One of the attributes of
 * game_state should be whether it has ended.*)
type game_state

(* move_spec is a format to specify a move or turn to be made.
 * E.g. choosing where to place an x in tic-tac-toe, or making a move in chess*)
type move_spec

(* [start_game] returns an initialized game state that is the starting point for
 * whichever game we’re playing (for now, tic tac toe) *)
val start_game: unit -> game_state

(* [update state move] takes in the current game state and the move that is
 * to be made, and updates game state. Returns unit *)
val update: game_state -> unit

(* [state_to_string state] takes in a game_state [state] and returns a string
 * representation of it for printing. *)
val state_to_string : game_state -> string

(* [update_state_and_to_string cmd state] updates state based on string
 * command cmd, where cmd is in the form x,y with any number of spaces.
 * Then it returns the string form of the state which is easy for printing.
 *)
 val update_state_and_to_string : string -> game_state -> string
