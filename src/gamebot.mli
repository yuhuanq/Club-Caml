(* game_state is the current state of the game. One of the attributes of
 * game_state should be whether it has ended.*)
type game_state


(* move_spec is a format to specify a move or turn to be made.
 * E.g. choosing where to place an x in tic-tac-toe, or making a move in chess*)
type move_spec


(* Initializes a game state that is the starting point for whichever
 * game we’re playing*)
val start_game: unit -> game_state


(* Takes in the current game state and the move that is to be made,
 * and returns the new game state*)
val make_move: (move_spec * game_state) -> game_state


(* [return_status] takes in a [game_state] and returns a string
 * representation of the game_state *)
val state_to_string : game_state -> string
