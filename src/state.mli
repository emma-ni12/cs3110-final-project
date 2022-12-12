(** Representation of the game state.

    This module represents the state of the game, including the board (which
    contains the marble positions), the number of players, and the player for
    the current turn. *)

type t
(** The abstract type of the value representing the game state. *)

exception BadPlayer
(** Raised when an number is not between 1 to 6 or when the color is not valid. *)

val init_state : Board.t -> int -> int list -> t
(** [init_state b p] is the initial state of the game with an initial board [b]
    for [p] players. The current turn is assigned to the player number 1, red.
    Requires: [p] is between 1 and 6, inclusive. *)

val current_board : t -> Board.t
(** [current_board st] is the board of the current state [st].*)

val current_player : t -> string
(** [current_player st] is player color associated with the current active turn
    in state [st].*)

val num_players : t -> int
(** [num_players st] is the number of players associated with the current active
    turn in state [st]. *)

val last_marble : t -> Board.m
(** [last_marble st] is the last moved marble associated with the current active
    turn in state [st]. *)

val last_player : t -> string
(** [last_player st] is the last player to go before the current active turn in
    state [st]. *)

val scoreboard : t -> int list
(** [scoreboard st] is the set of current scores for each player in state [st].*)

(** The type representing the result of an attempted movement. [Illegal] carries
    the state of the board with no changes made and an message regarding the
    reason for the illegality. [Legal st' auto_end game_won] carries the state
    of the board after the legal move, a condition controlling whether or not
    the player's turn should end automatically, and a condition dictating
    whether the player has won the game.*)
type result =
  | Legal of (t * bool * bool)
  | Illegal of (t * string)

val move : int -> string -> t -> result
(** [move m dir st] is the result of attempting to move marble [m] in the
    direction [dir] on the board in current state [st].

    - If [dir] is not in ["L"; "R"; "LU"; "RU"; "LD"; "RD"], [m] is not between
      1 and 10 inclusive, [m] is a different marble than one moved by the same
      player earlier in the same turn, or the destination coordinate in the
      desired direciton [dir] is occupied/off the board, the result is [Illegal]
      with a corresponding error message. The result is also [Illegal] if a
      player attempts to move a marble to an adjacent hole after doing one or
      more hops.

    - Otherwise, the result is [Legal]. If the attempted move was to move a
      marble to an adjacent hole, the player's turn automatically ends, and the
      first bool that [Legal] carries is [true]. If all 10 of the player's
      marbles are in the opposite corner, the player wins, and the second bool
      that [Legal] carries is [true]. Otherwise, these conditions are [false]. *)

val end_turn : t -> t
(** [end_turn st] is the new state of ending the current turn and advancing the
    game play to the next player's turn. *)
