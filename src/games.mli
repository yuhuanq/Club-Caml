(* game_state is the current state of the game. *)
type t

(* move_spec is a format to specify a move or turn to be made.
 * E.g. choosing where to place an x in tic-tac-toe, or making a move in chess*)
type move_spec

val is_over : t -> bool

val instructions : string

(* [start_game] returns an initialized game state that is the starting point for
 * whichever game we’re playing (for now, tic tac toe) *)
val new_game: unit -> t

(* [game_state_to_string state] takes in a game_state [state] and returns a string
 * representation of it for printing. *)
val to_string : t -> string

(* [give_updated_game_state cmd state] updates state based on string
 * command cmd, where cmd is in the form x,y with any number of spaces.
 * Then it returns the state. *)
val play : t -> string -> t

