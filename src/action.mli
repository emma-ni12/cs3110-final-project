(** Parsing of player actions. *)

type action =
  | Move of (int * string)
  | End
  | Quit
      (** type [action] represents a legal action that a player can take during
          a game. These include moving a marble, ending a turn, or quitting the
          game. Invariant: [Move] must take an integer between 1 and 10, and a
          legal direction code (L, LU, LD, R, RU, RD). *)

exception Empty
(** Raised when an empty action is parsed. *)

exception Invalid_Action
(** Raised when a invalid action is parsed. *)

val parse : string -> action
(** [parse a] parses a player's input into an [action]. The first word of the
    input is the action verb, and any remaining words are used for action data.
    Examples:

    - [parse "move 10 L"] is [Move (10, "L")].
    - [parse "end"] is [End].
    - [parse "quit"] is [Quit].

    Requires: [a] contains only alphanumeric (a-z, A-Z, 0-9) and space
    characters (ASCII code 32).

    Raises: [Empty] if [a] is the empty string or contains only spaces.

    Raises: [Invalid_Action] if the action is invalid. An action is invalid if
    its first word is not "move", "end", or "quit"; if the first word is "move"
    but the action does not include a number between 1 and 10 and a direction
    code (L, LU, LD, R, RU, RD); or if the first word is "end" or "quit" and the
    action contains characters after the first word. *)
