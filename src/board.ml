open Yojson.Basic.Util

type m = {
  color : string;
  number : int;
}

type section = {
  color : string;
  coords : (int * int) list;
}
(** [section] is color and coordinates making up one of the 6 corners, or the
    middle of the board. *)

type t = {
  board : ((int * int) * m option) list;
  sections : section list;
}

exception BadJson
exception BadCoord of (int * int)
exception BadMarble

(** [un_yojson_list j] turns the list of coordinates from yojson to OCaml*)
let un_yojson_list j =
  match j with
  | color, coords_yojson -> (color, to_list coords_yojson)

(** [list_to_pair \[x; y\]] turns the list of coordinates into a pair of
    coordinates*)
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

(* can refactor/combine these functions*)

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
    is inputed number of players. *)
let create_section_holes p n color lst =
  if p >= n then coord_list_to_marble_list lst color 1
  else coord_list_to_hole_list lst color

(** [create_holes p j] are the holes associated with all sections (corners and
    center) included in [j], populated for a number of [p] players. *)
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

(** [create_section_coords j] is a section of a color and its associated
    coordinate points matched in [j]. *)
let create_section_coords j =
  match j with
  | "red", lst -> { color = "red"; coords = lst }
  | "black", lst -> { color = "black"; coords = lst }
  | "yellow", lst -> { color = "yellow"; coords = lst }
  | "blue", lst -> { color = "blue"; coords = lst }
  | "white", lst -> { color = "white"; coords = lst }
  | "green", lst -> { color = "green"; coords = lst }
  | "center", lst -> { color = "center"; coords = lst }
  | _ -> raise BadJson

(* let init_board' players = let j = Yojson.Basic.from_file "./data/coords.json"
   |> to_assoc |> List.map un_yojson_list |> List.map yojson_list_to_tuple in
   let holes_by_color = List.map (create_holes players) j in List.fold_left (fun
   lst1 lst2 -> lst1 @ lst2) [] holes_by_color *)

let init_board players =
  let j =
    Yojson.Basic.from_file "./data/coords.json"
    |> to_assoc |> List.map un_yojson_list
    |> List.map yojson_list_to_tuple
  in
  let holes_by_color = List.map (create_holes players) j in
  let b = List.fold_left (fun lst1 lst2 -> lst1 @ lst2) [] holes_by_color in
  let sec = List.map create_section_coords j in
  { board = b; sections = sec }

let rec holes_with_marbles_helper b =
  match b with
  | [] -> []
  | ((x, y), m_opt) :: tail ->
      if m_opt <> None then (x, y) :: holes_with_marbles_helper tail
      else holes_with_marbles_helper tail

let holes_with_marbles (t : t) = holes_with_marbles_helper t.board

(* let rec holes_with_marbles' t = match t with | [] -> [] | ((x, y), m_opt) ::
   tail -> if m_opt <> None then (x, y) :: holes_with_marbles tail else
   holes_with_marbles tail*)

let rec empty_holes_helper b =
  match b with
  | [] -> []
  | ((x, y), m_opt) :: tail ->
      if m_opt = None then (x, y) :: empty_holes_helper tail
      else empty_holes_helper tail

let rec empty_holes (t : t) = empty_holes_helper t.board

(* let rec empty_holes' t = match t with | [] -> [] | ((x, y), m_opt) :: tail ->
   if m_opt = None then (x, y) :: empty_holes tail else empty_holes tail*)

let marble_in_hole (t : t) (x, y) =
  try List.assoc (x, y) t.board with Not_found -> raise (BadCoord (x, y))

let rec coord_of_marble (b : t) (m : m) =
  match b.board with
  | [] -> raise BadMarble
  | ((x, y), m_opt) :: t ->
      if m_opt = Some m then (x, y)
      else coord_of_marble { board = t; sections = b.sections } m

(** [marble_match_color color marble] is whether the color of [marble] matches
    [color]. If [marble] is None, it's false. *)
let marble_match_color (color : string) (marble : m option) : bool =
  match marble with
  | Some m -> m.color = color
  | None -> false

(** [corner_marble_colors t c lst] is whether all the coordinates in [lst]
    contain marbles on the board [t] that are of the color [c]. *)
let rec corner_marble_colors (t : t) (c : string) lst =
  match lst with
  | [] -> true
  | coord :: tail ->
      marble_in_hole t coord |> marble_match_color c
      && corner_marble_colors t c tail

let rec all_in_corner (b : t) (corner_c : string) (marble_c : string) : bool =
  match b.sections with
  | [] -> raise BadMarble
  | sec :: tail ->
      if sec.color = corner_c then corner_marble_colors b marble_c sec.coords
      else all_in_corner { board = b.board; sections = tail } corner_c marble_c

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

let string_of_board (b : t) = string_of_board_helper b 1 1

let edit_board_at_coord b (x, y) (new_marble : m option) =
  let new_b =
    List.map
      (fun ((a, b), m) ->
        if a = x && b = y then ((a, b), new_marble) else ((a, b), m))
      b.board
  in
  { board = new_b; sections = b.sections }
