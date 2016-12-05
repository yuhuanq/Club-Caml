(*
 * games.ml
 * Copyright (C) 2016 Byungchan Lim <bl458@cornell.edu>
 *
 * Distributed under terms of the MIT license.
 *)

(* For now games.ml only has tictactoe. Will expand games by making a general
Games module with submodules like module Tictactoe. *)

(* t is the current state of the game. *)
type t = {
  mutable ended : bool;
  mutable turns : int;
  grid : bool option array array;
}

let is_over t = t.ended

let instructions =
  "Tic Tack Toe:
    The usual rules.

    The board is a 3x3 matrix
    i,j -> places an X or O at cell i,j
    i.e.
    2,3 or 1,1\n"


(* move_spec is a format to specify a move or turn to be made.
 * E.g. choosing where to place an x in tic-tac-toe, or making a move in chess
 *)
type move_spec =
| O of int * int
| X of int * int

(* [new_game] returns an initialized game state that is the starting point for
 * whichever game we’re playing (for now, tic tac toe) *)
let new_game () =
{
  ended = false;
  turns = 0;
  grid = Array.make_matrix 3 3 None
}

(* [ended_or_not state] updates state based on whether game has ended or not. *)
let ended_or_not state =
  let sz = Array.length state.grid in
  for x = 0 to sz - 1 do
    for y = 0 to sz - 1 do
      if x-1 >= 0 && x + 1 <= sz - 1
        && state.grid.(x-1).(y) = state.grid.(x).(y)
        && state.grid.(x).(y) = state.grid.(x+1).(y)
        && state.grid.(x).(y) != None
        then state.ended <- true
      else if y - 1 >= 0 && y + 1 <= sz - 1
        && state.grid.(x).(y-1) = state.grid.(x).(y)
        && state.grid.(x).(y) = state.grid.(x).(y+1)
        && state.grid.(x).(y) != None
        then state.ended <- true
      else
        ()
    done;
  done;
  ()

(* [update state move] takes in the current game state and the move that is
 * to be made, and updates game state. Returns unit *)
let update_on_move state move =
  match move with
  | O (x, y) ->
    if state.grid.(x).(y) = None
      then
      (state.grid.(x).(y) <- Some true;
      state.turns <- state.turns + 1;
      ended_or_not state)
    else
      ()
  | X (x, y) ->
    if state.grid.(x).(y) = None
      then
      (state.grid.(x).(y) <- Some false;
      state.turns <- state.turns + 1;
      ended_or_not state)
    else
      ()

(* Methods for printing game state to terminal screen *)

(* [one_box_to_string b color] takes in a single box of a tic-tac-toe grid
 * object and its color and returns a string representation of it for printing.
 *)
let one_box_to_string b =
  match b with
  | None -> " "
  | Some symbol ->
    if symbol = true then "O"
    else "X"

(* [state_to_string state] takes in a t [state] and returns a string
 * representation of it for printing. *)
let to_string state =
  let sz = Array.length state.grid in
  let buff = Buffer.create ((sz+1)*(sz+1)) in
  for x = 0 to sz - 1 do
    for y = 0 to sz - 1 do
      Buffer.add_char buff '|';
      Buffer.add_string buff (one_box_to_string state.grid.(x).(y));
      if y=sz-1 then Buffer.add_char buff '|'
    done;
    Buffer.add_string buff "\n"
  done;
  Buffer.contents buff

(* [print_state state] takes in a state and prints/draws it to the terminal.
 * For debugging. And for when client prints state maybe
 *)
let print_state state =
  state |> to_string |> print_string

(* [play state cmd] updates state based on string
 * command cmd, where cmd is in the form x,y with any number of spaces.
 * Then it returns the state. *)
let play state cmd =
  let cmd' = Str.global_replace (Str.regexp_string " ") "" cmd in
  let (x,y) =
    (int_of_string (String.sub cmd' 0 1),
    int_of_string(String.sub cmd' 2 1)) in
  if state.turns - (state.turns / 2) * 2 = 1 (* mod *)
    then (update_on_move state (O(x,y)); state)
  else
    (update_on_move state (X(x,y)); state)
