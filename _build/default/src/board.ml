open Yojson.Basic.Util

type m = {
  color : string;
  number : int;
}

type t = ((int * int) * m option) list

exception BadJson
exception BadCoord of (int * int)

let un_yojson_list j =
  match j with
  | color, coords_yojson -> (color, to_list coords_yojson)

let list_to_pair = function
  | [ x; y ] -> (to_int x, to_int y)
  | _ -> raise BadJson

let yojson_list_to_tuple j =
  match j with
  | color, coords ->
      let un_yojson = List.map to_list coords in
      let coord_tuples = List.map list_to_pair un_yojson in
      (color, coord_tuples)

let rec coord_list_to_marble_list lst color c =
  match lst with
  | [] -> []
  | (x, y) :: t ->
      ((x, y), Some { color; number = c })
      :: coord_list_to_marble_list t color (c + 1)

let rec coord_list_to_hole_list lst color =
  match lst with
  | [] -> []
  | (x, y) :: t -> ((x, y), None) :: coord_list_to_hole_list t color

let create_holes p j =
  match j with
  | "red", lst ->
      if p >= 1 then coord_list_to_marble_list lst "red" 1
      else coord_list_to_hole_list lst "red"
  | "black", lst ->
      if p >= 2 then coord_list_to_marble_list lst "black" 1
      else coord_list_to_hole_list lst "black"
  | "yellow", lst ->
      if p >= 3 then coord_list_to_marble_list lst "yellow" 1
      else coord_list_to_hole_list lst "yellow"
  | "blue", lst ->
      if p >= 4 then coord_list_to_marble_list lst "blue" 1
      else coord_list_to_hole_list lst "blue"
  | "white", lst ->
      if p >= 5 then coord_list_to_marble_list lst "white" 1
      else coord_list_to_hole_list lst "white"
  | "green", lst ->
      if p >= 6 then coord_list_to_marble_list lst "green" 1
      else coord_list_to_hole_list lst "green"
  | "center", lst -> coord_list_to_hole_list lst "center"
  | _ -> raise BadJson

let init_board ?(players = 1) () =
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

let marble_in_hole t (x, y) =
  try List.assoc (x, y) t with Not_found -> raise (BadCoord (x, y))

(* [("red", [(1,1); (1,2); (1,3)]); ("blue", [(4,2), (3,2)])] red and p>=1 then
   fill the holes black and p>=2 then fill the holes ((1,2), None / Some m) *)

(* let rec print_board (lst : t) = match lst with | [] -> "" | ((x, y), m_opt)
   :: t -> ( match m_opt with | None -> "(" ^ string_of_int x ^ "," ^
   string_of_int y ^ ") , " ^ print_board t | Some m -> "(" ^ string_of_int x ^
   "," ^ string_of_int y ^ ") " ^ m.color ^ ", " ^ print_board t) *)
