(* game_state is the current state of the game. *)
type game_state

(* move_spec is a format to specify a move or turn to be made.
 * E.g. choosing where to place an x in tic-tac-toe, or making a move in chess*)
type move_spec

(* [start_game] returns an initialized game state that is the starting point for
 * whichever game we’re playing (for now, tic tac toe) *)
val start_game: unit -> game_state

(* [update_on_move state move] updates the game state based on the move that is
 * to be made. Returns unit *)
val update_on_move: game_state -> move_spec -> unit

(* [game_state_to_string state] takes in a game_state [state] and returns a string
 * representation of it for printing. *)
val game_state_to_string : game_state -> string

(* [give_updated_game_state cmd state] updates state based on string
 * command cmd, where cmd is in the form x,y with any number of spaces.
 * Then it returns the state. *)
 val give_updated_game_state : string -> game_state -> game_state
