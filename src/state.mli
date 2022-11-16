(** Representation of the game

    This module represents the state of the game, including the board (which
    contains the marble positions), the number of players, and the player for
    the current turn. *)

type t
(** The abstract type of the value representing the game state. *)

exception BadPlayer
(** Raised when an number is not between 1 to 6 or when the color is not valid.
    It carries the invalid coordinate. *)

val init_state : Board.t -> int -> t
(** [init_state b p] is the initial state of the game with an initial board [b]
    for [p] players. The current turn is assigned to the player number 1, red.
    Requires: [p] is between 1 and 6, inclusive. *)

val current_board : t -> Board.t
(** [current_board st] is the current state [st] of the board.*)

val current_player : t -> string
(** [current_player st] is the current player color associated with the current
    active turn.*)

val num_players : t -> int
(** [num_players st] is the current number of players associated with the
    current active turn*)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of (t * bool)
  | Illegal of (t * string)

val move : int -> string -> t -> result
(** [move m dir st] is the result of attempting to move marble [m] in the
    direction [dir] on the board in current state [st]. *)

(** TALK ABOUT LEGAL VS ILLEGAL IN SPECS, illegal just keep current state *)

val end_turn : t -> t
(** [end_turn st] is the new state of ending the current turn and advancing the
    game play to the next player's turn. *)
