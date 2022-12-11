open Board
open Yojson.Basic.Util

type t = {
  board : Board.t;
  num_players : int;
  current_player : string;
  last_marble : Board.m;
}

type result =
  | Legal of (t * bool * bool)
  | Illegal of (t * string)

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

(** [wrap n] is the player number [n], wrapped between 1-6. *)
let wrap p n = if n > p then 1 else n

let init_state b p =
  {
    board = b;
    num_players = p;
    current_player = color_from_number player_order 1;
    last_marble = { color = "none"; number = -1 };
  }

let current_board st = st.board
let current_player st = st.current_player
let num_players st = st.num_players
let last_marble st = st.last_marble

type hole_status =
  | Open
  | Occupied
  | OffBoard

(** [valid_destination st d] is whether [d] is a valid destination on the
    current board in [st]. A valid destination is one that exists on the board
    and isn't currently occupied by another marble. *)
let valid_destination (st : t) (d : int * int) =
  match marble_in_hole (current_board st) d with
  | None -> Open
  | Some _ -> Occupied
  | exception BadCoord _ -> OffBoard

let rec valid_hop (x, y) (x', y') st =
  match valid_destination st (x + x', y + y') with
  | Open -> (x + x', y + y')
  | Occupied -> valid_hop (x + x', y + y') (x', y') st
  | OffBoard -> raise (BadCoord (x, y))

(** [hop (x,y) dir st] is the destination coordinate after attempting to move in
    direction [dir] from coordinate [(x,y)]. Requires: [dir] is a valid
    direction. *)
let hop (x, y) (dir : string) (st : t) =
  match dir with
  | "L" -> (
      match valid_hop (x, y) (-2, 0) st with
      | x', y' -> if x - 2 = x' && y = y' then (x, y) else (x', y')
      | exception BadCoord _ -> (x, y))
  | "R" -> (
      match valid_hop (x, y) (2, 0) st with
      | x', y' -> if x + 2 = x' && y = y' then (x, y) else (x', y')
      | exception BadCoord _ -> (x, y))
  | "LU" -> (
      match valid_hop (x, y) (-1, -1) st with
      | x', y' -> if x - 1 = x' && y - 1 = y' then (x, y) else (x', y')
      | exception BadCoord _ -> (x, y))
  | "RU" -> (
      match valid_hop (x, y) (1, -1) st with
      | x', y' -> if x + 1 = x' && y - 1 = y' then (x, y) else (x', y')
      | exception BadCoord _ -> (x, y))
  | "LD" -> (
      match valid_hop (x, y) (-1, 1) st with
      | x', y' -> if x - 1 = x' && y + 1 = y' then (x, y) else (x', y')
      | exception BadCoord _ -> (x, y))
  | "RD" -> (
      match valid_hop (x, y) (1, 1) st with
      | x', y' -> if x + 1 = x' && y + 1 = y' then (x, y) else (x', y')
      | exception BadCoord _ -> (x, y))
  | _ -> (x, y)

(** *)
let valid_slide (x, y) (x', y') (st : t) =
  match valid_destination st (x + x', y + y') with
  | Open -> (x + x', y + y')
  | Occupied -> (x, y)
  | OffBoard -> (x, y)

(** [slide (x,y) dir st] is the destination coordinate reached after moving to
    the adjacent coordinate in direction [dir]. *)
let slide (x, y) (dir : string) (st : t) =
  match dir with
  | "L" -> valid_slide (x, y) (-2, 0) st
  | "R" -> valid_slide (x, y) (2, 0) st
  | "LU" -> valid_slide (x, y) (-1, -1) st
  | "RU" -> valid_slide (x, y) (1, -1) st
  | "LD" -> valid_slide (x, y) (-1, 1) st
  | "RD" -> valid_slide (x, y) (1, 1) st
  | _ -> (x, y)

let opposite_corner color =
  match color with
  | "red" -> "black"
  | "black" -> "red"
  | "yellow" -> "blue"
  | "blue" -> "yellow"
  | "white" -> "green"
  | "green" -> "white"
  | _ -> failwith "Invalid color"

(** [win_condition_met st] is whether the current player in [st] has won the
    game. The player has won if they've moved all of their marbles into the
    opposite corner. *)
let win_condition_met st : bool =
  let corner_to_check = st.current_player |> opposite_corner in
  all_in_corner st.board corner_to_check st.current_player

(**[calculate_move m (x',y') t] is the result of moving the marble to a certain
   destination *)
let calculate_move (m : int) (dir : string) (st : t) =
  let coord =
    coord_of_marble (current_board st) { color = st.current_player; number = m }
  in
  let new_st =
    { st with board = edit_board_at_coord (current_board st) coord None }
  in
  (* calculate the destination coordinate*)
  let destination =
    if { color = st.current_player; number = m } = st.last_marble then
      hop coord dir st (* if a player has already moved, they can only hop*)
    else
      match hop coord dir st with
      | x, y -> if (x, y) = coord then slide coord dir st else (x, y)
    (* if a player has not moved yet, they can hop or slide*)
  in
  match destination <> coord with
  | true ->
      let displacement =
        abs (fst destination - fst coord) + abs (snd destination - snd coord)
      in
      let auto_end = if displacement <= 2 then true else false in
      let final_st =
        {
          new_st with
          board =
            edit_board_at_coord (current_board new_st) destination
              (Some { color = st.current_player; number = m });
          last_marble = { color = st.current_player; number = m };
        }
      in
      let game_won = win_condition_met final_st in
      Legal (final_st, auto_end, game_won)
  | false ->
      Illegal
        ( st,
          "You can't move off the board, into an occupied hole, or try to move \
           to an adjacent hole after doing one or more hops over marbles; try \
           again!" )

let move (m : int) (dir : string) (st : t) =
  if m < 1 || m > 10 then Illegal (st, "Pick a marble between 1 and 10!")
  else
    match st.last_marble with
    | { color; number } -> (
        if color = st.current_player && m <> number then
          Illegal (st, "You can only move one marble per turn!")
          (* check if player has already moved one of their marbles this turn*)
        else
          match dir with
          | "L" | "R" | "RU" | "LU" | "RD" | "LD" -> calculate_move m dir st
          | _ ->
              Illegal
                ( st,
                  "Choose a direction: L (left), R (right), LU (left-up), RU \
                   (right-up), LD (left-down), RD (right-down)." ))

let end_turn st =
  let next_player =
    number_from_color player_order st.current_player + 1
    |> wrap st.num_players
    |> color_from_number player_order
  in
  { st with current_player = next_player }
