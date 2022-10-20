(** Representation of the game

    This module represents the state of the game, including the board (which
    contains the marble positions), the number of players, and the player for
    the current turn. *)

type t
(** The abstract type of the value representing the game state. *)

val init_state : Board.t -> int -> t
(** [init_state b p] is the initial state of the game with an initial board [b]
    for [p] players. The current turn is assigned to the player number 1, red.
    Requires: [p] is between 1 and 6, inclusive. *)

val current_board : t -> Board.t
(** [current_board st] is the current state [st] of the board.*)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal

val move : string -> string -> t -> result
(** [move m dir st] is the result of attempting to move marble [m] in the
    direction [dir] on the board in current state [st]. *)

val end_turn : t -> result
(** [end_turn st] is the result of ending the current turn and advancing the
    game play to the next player's turn. *)
