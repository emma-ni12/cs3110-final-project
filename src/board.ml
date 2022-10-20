open Yojson.Basic.Util

type m = {
  color : string;
  number : int;
}

type t = ((int * int) * m option) list

exception BadJson
exception BadCoord of (int * int)
exception BadMarble

(** [un_yojson_list j] turns the list of coordinates from yojson to OCaml*)
let un_yojson_list j =
  match j with
  | color, coords_yojson -> (color, to_list coords_yojson)

(** [list_to_pair] turns the list of coordinates into a pair of coordinates*)
let list_to_pair = function
  | [ x; y ] -> (to_int x, to_int y)
  | _ -> raise BadJson

(** [yojson_list_to_tuple j] converts the coordinates from lists to tuples*)
let yojson_list_to_tuple j =
  match j with
  | color, coords ->
      let un_yojson = List.map to_list coords in
      let coord_tuples = List.map list_to_pair un_yojson in
      (color, coord_tuples)

(** [coord_list_to_marble_list lst color c] take a coordinate and return a tuple
    with the coordinates and a new marble with color and c*)
let rec coord_list_to_marble_list lst color c =
  match lst with
  | [] -> []
  | (x, y) :: t ->
      ((x, y), Some { color; number = c })
      :: coord_list_to_marble_list t color (c + 1)

(** can refactor/combine these functions*)

(** [coord_list_to_hole_list lst color] take a coordinate and return a tuple
    with the coordinates and an empty hole*)
let rec coord_list_to_hole_list lst color =
  match lst with
  | [] -> []
  | (x, y) :: t -> ((x, y), None) :: coord_list_to_hole_list t color

(** [string_of_number n] is the two-digit string representation of the number
    [n]. If [n] is less than 10, then a 0 is added in front. Example:
    [string_of_number 1] is "01". Requires: 0 <= n <= 99 *)
let string_of_number n =
  if n < 10 then "0" ^ string_of_int n else string_of_int n

(** [create_section_holes] are the holes associated with a corner of the board,
    populated by [color] marbles, [n] is number of players we check against, [p]
    is inputed number of players.

    INCOMPLETE SPEC, COME BACK *)
let create_section_holes p n color lst =
  if p >= n then coord_list_to_marble_list lst color 1
  else coord_list_to_hole_list lst color

(** WRITE SPEC HERE *)
let create_holes p j =
  match j with
  | "red", lst -> create_section_holes p 1 "red" lst
  | "black", lst -> create_section_holes p 2 "black" lst
  | "yellow", lst -> create_section_holes p 3 "yellow" lst
  | "blue", lst -> create_section_holes p 4 "blue" lst
  | "white", lst -> create_section_holes p 5 "white" lst
  | "green", lst -> create_section_holes p 6 "green" lst
  | "center", lst -> coord_list_to_hole_list lst "center"
  | _ -> raise BadJson

let init_board players =
  let j =
    Yojson.Basic.from_file "./data/coords.json"
    |> to_assoc |> List.map un_yojson_list
    |> List.map yojson_list_to_tuple
  in
  let holes_by_color = List.map (create_holes players) j in
  List.fold_left (fun lst1 lst2 -> lst1 @ lst2) [] holes_by_color

let rec holes_with_marbles t =
  match t with
  | [] -> []
  | ((x, y), m_opt) :: tail ->
      if m_opt <> None then (x, y) :: holes_with_marbles tail
      else holes_with_marbles tail

let rec empty_holes t =
  match t with
  | [] -> []
  | ((x, y), m_opt) :: tail ->
      if m_opt = None then (x, y) :: empty_holes tail else empty_holes tail

let marble_in_hole (t : t) (x, y) =
  try List.assoc (x, y) t with Not_found -> raise (BadCoord (x, y))

let rec coord_of_marble (b : t) (m : m) =
  match b with
  | [] -> raise BadMarble
  | ((x, y), m_opt) :: t ->
      if m_opt = Some m then (x, y) else coord_of_marble t m

let rec string_of_board_helper b row col =
  match (row, col) with
  | 18, _ -> ""
  | r, 26 -> "\n \n" ^ string_of_board_helper b (r + 1) 1
  | r, c ->
      (match marble_in_hole b (c, r) with
      | exception BadCoord _ -> "    "
      | Some m -> "(" ^ string_of_number m.number ^ ")"
      | None -> "(  )")
      ^ string_of_board_helper b r (c + 1)

let string_of_board b = string_of_board_helper b 1 1

let edit_board_at_coord b (x, y) (new_marble : m option) =
  List.map
    (fun ((a, b), m) ->
      if a = x && b = y then ((a, b), new_marble) else ((a, b), m))
    b
