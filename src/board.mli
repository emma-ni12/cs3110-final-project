(** Representation of the star shaped board

    This module represents the state of a board, including the holes on the
    board, and marbles in holes. *)

type t
(** The abstract type of values representing the physical board. *)

type m = {
  color : string;
  number : int;
}
(** The type of the marble. *)

exception BadCoord of (int * int)
(** Raised when an invalid coordinate is encountered. It carries the invalid
    coordinate. *)

val init_board : ?players:int -> unit -> t
(** [init_board a] is the initial state of the board with [a] number of players.
    In that state the baord is initialized with 10 marbles for each player in
    the corners of the star, starting from the bottom point. If there are
    multiple players, the second will be top, the third will be top left, the
    fourth will be bottom right, the fifth will be bottom left, and the sixth
    will be top right. Requires: [players] is an integer between 1 and 6,
    inclusive. *)

val holes_with_marbles : t -> (int * int) list
(** [holes_with_marbles b] is the list of coordinates corresponding to holes
    that currently have marbles. *)

val empty_holes : t -> (int * int) list
(** [empty_holes b] is the list of coordinates corresponding to holes that do
    not currently have marbles. *)

val marble_in_hole : t -> int * int -> m option
(** [marble_in_hole b coord] is the marble at hole [coord] on the board b. If
    the hole does contain a marble, the result is [Some m]. Otherwise, the
    result is [None]. Raises [BadCoord coord] if [coord] is not a valid
    coordinate for a hole on the board.*)

(* val print_board : t -> string *)
