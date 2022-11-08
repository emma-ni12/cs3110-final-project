open Board
open Yojson.Basic.Util

type t = {
  board : Board.t;
  num_players : int;
  current_player : string;
}

type result =
  | Legal of t
  | Illegal of t

exception BadPlayer

(** [player_order] is the map of players, matching a player's color to their
    order of turn in the game. *)
let player_order =
  Yojson.Basic.from_file "./data/player_order.json"
  |> to_assoc
  |> List.map (fun (x, y) -> (x, to_int y))

(** [number_from_color color players] is the number associated with color
    [color] from the [players]. Raises BadPlayer if the color is not a valid
    color. *)
let rec number_from_color players (color : string) =
  match players with
  | [] -> raise BadPlayer
  | (c, n) :: t -> if c = color then n else number_from_color t color

(** [color_from_number number players] is the color associated with the player
    [number] from the [players]. Raises BadPlayer if the number is not a valid
    number of players. *)
let rec color_from_number players number =
  match players with
  | [] -> raise BadPlayer
  | (c, n) :: t -> if n = number then c else color_from_number t number

let init_state b p =
  {
    board = b;
    num_players = p;
    current_player = color_from_number player_order 1;
  }

let current_board st = st.board
let current_player st = st.current_player
let num_players st = st.num_players

(** [valid_destination st d] is whether [d] is a valid destination on the
    current board in [st]. A valid destination is one that exists on the board
    and isn't currently occupied by another marble. *)
let valid_destination (st : t) (d : int * int) =
  match marble_in_hole (current_board st) d with
  | None -> true
  | Some _ | (exception BadCoord _) -> false

(**[calculate_move m (x',y') t] is the result of moving the marble to a certain
   destination *)
let calculate_move (m : int) (x', y') (st : t) =
  let coord =
    coord_of_marble (current_board st) { color = st.current_player; number = m }
  in
  let new_st =
    { st with board = edit_board_at_coord (current_board st) coord None }
  in
  let destination =
    match coord with
    | x, y -> (x + x', y + y')
  in
  match valid_destination st destination with
  | true ->
      Legal
        {
          new_st with
          board =
            edit_board_at_coord (current_board new_st) destination
              (Some { color = st.current_player; number = m });
        }
  | false -> Illegal st

(* right now, all moves are "legal," we will implement the rule checking
   after *)
let move (m : int) (dir : string) (st : t) =
  if m < 1 || m > 10 then Illegal st
  else
    match dir with
    | "L" -> calculate_move m (-2, 0) st
    | "R" -> calculate_move m (2, 0) st
    | "LU" -> calculate_move m (-1, -1) st
    | "RU" -> calculate_move m (1, -1) st
    | "LD" -> calculate_move m (-1, 1) st
    | "RD" -> calculate_move m (1, 1) st
    | _ -> Illegal st

(** [wrap n] is the player number [n], wrapped between 1-6. *)
let wrap n = if n > 6 then 1 else n

let end_turn st =
  let next_player =
    number_from_color player_order st.current_player + 1
    |> wrap
    |> color_from_number player_order
  in
  { st with current_player = next_player }
