(** Test Plan:

    Given that our group created a game (Chinese Caml Checkers), our testing
    approach consisted of both extensive play/manual testing and OUnit testing
    (tests in this file) to make sure certain features worked the way we
    expected them to.

    More specifically, we tested the board (shape, valid/invalid holes), state,
    destination validity, and sequences of movements with OUnit testing and we
    tested our win condition, ability to move marbles, score count, and checking
    if the game directions showed up at the beginning of the game with manual
    testing. Since the board, state, movements, and destination validity all
    contains pieces of logic that are needed to play the game and methods that
    modify the game state accordingly, we decided that it would be helpful to
    test all of these functions with OUnit tests. For functionality like win
    condition and ability to move marbles around the board, we felt like they
    were either easier to test through play testing or they were already
    indirectly tested through other functions.

    For features that we tested with OUnit testing, we employed the concepts of
    glass-box and black-box testing. One group member would write a function and
    test cases based on their implementation of the function (glass-box) and
    another group member would then write tests based only on the specifications
    of the function defined in the mli (black-box). We did not use randomized
    testing because our game system operates on predictable movements based on a
    set of predefined rules. Additionally, we felt that play testing our game
    helped us make better implementation decisions and customize user experience
    to achieve a desired output in specific situations.

    We believe that our test suite demonstrates the correctness of our systems
    because every function listed within our mli files was tested. We used
    property-based tests and checked if the correct outputs based on
    specifications were returned to ensure that the functions listed modify the
    game state properly. We also used the Bisect tool on our test suite to make
    sure all our files have above 90% test coverage. We felt that this amount of
    code coverage was enough for us to feel confident about our test suites,
    since some cases that we didn't test are considered unreachable. *)

open OUnit2
open Game
open Board
open Action
open State
open Yojson.Basic.Util

(*************************************************************************)
(* PRINTERS AND HELPERS *)
(*************************************************************************)

(** [string_of_int_pair_list lst] is the string representation of a list of
    int*int pairs *)
let rec string_of_int_pair_list = function
  | [] -> ""
  | (x, y) :: t ->
      "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ") , "
      ^ string_of_int_pair_list t

(** [string_of_marble_option m] is the string representation of type m *)
let string_of_marble_option = function
  | None -> "empty hole"
  | Some { color; number } -> color ^ string_of_int number

(** [string_of_coord c] is the string representation of a coordinate [c]. *)
let string_of_coord (x, y) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

(** [string_of_state st] is the string representation of a state [st]. *)
let string_of_state (st : State.t) =
  let n_p = num_players st in
  let current_p = current_player st in
  let last_m = last_marble st in
  "State: board, number of players: " ^ string_of_int n_p ^ ", current player: "
  ^ current_p ^ ", last marble: "
  ^ string_of_marble_option (Some last_m)

(** [string_of_action a] is the string representation of an action [a]. *)
let string_of_action (a : action) =
  match a with
  | Move (n, d) -> "Move " ^ string_of_int n ^ " " ^ d
  | End -> "End"
  | Quit -> "Quit"

(** [string_of_result r] is the string representation of the result [r]. *)
let string_of_result = function
  | Illegal (t, message) -> "Illegal: old state, message: " ^ message
  | Legal (t, auto_end, game_won) ->
      "Legal: new state, auto end: " ^ string_of_bool auto_end ^ ", game won: "
      ^ string_of_bool game_won

(** [state_of_result r] is the state contained in the result [r]. *)
let state_of_result r =
  match r with
  | Legal (st, _, _) -> st
  | Illegal (st, _) -> st

(** [cmp_pair_lists lst1 lst2] compares two lists of pairs to see whether they
    are equivalent lists of pairs, regardless of order *)
let cmp_pair_lists lst1 lst2 = List.for_all (fun a -> List.mem a lst2) lst1

(** [list_to_pair \[x; y\]] turns the list of coordinates into a pair of
    coordinates*)
let list_to_pair = function
  | [ x; y ] -> (to_int x, to_int y)
  | _ -> failwith "bad testing json"

(** [yojson_list_to_tuple j] converts the coordinates from lists to tuples*)
let yojson_list_to_tuple j =
  let un_yojson = List.map to_list j in
  List.map list_to_pair un_yojson

type test_record = {
  test : string;
  number : int;
  coords : (int * int) list;
}

(** [un_yojson_list j] turns the list of coords & number from yojson to OCaml*)
let un_yojson_list j =
  match j with
  | test_name, data_yojson -> (
      match to_assoc data_yojson with
      | [ ("num", i); ("coords", c) ] ->
          let coords = yojson_list_to_tuple (to_list c) in
          { test = test_name; number = to_int i; coords }
      | _ -> failwith "bad testing json")

(*************************************************************************)
(* UNIT TEST FUNCTIONS *)
(*************************************************************************)

let init_marble_test_records =
  Yojson.Basic.from_file "./data/test_init.json"
  |> to_assoc |> List.map un_yojson_list

let init_empty_holes_records =
  Yojson.Basic.from_file "./data/test_empty.json"
  |> to_assoc |> List.map un_yojson_list

(** [init_marbles_test name p expected_output] constructs a OUnit test named
    [name] that asserts the equality of [expected_output] with
    [holes_with_marbles] for a board with p players. *)
let init_marbles_test test_record =
  match test_record with
  | { test; number; coords } ->
      test >:: fun _ ->
      assert_equal coords ~cmp:cmp_pair_lists
        (holes_with_marbles (init_board number))
        ~printer:string_of_int_pair_list

(** [init_empty_holes_test name p expected_output] constructs a OUnit test named
    [name] that asserts the equality of [expected_output] with [empty_holes] for
    a board with p players. *)
let init_empty_holes_test test_record =
  match test_record with
  | { test; number; coords } ->
      test >:: fun _ ->
      assert_equal coords ~cmp:cmp_pair_lists
        (empty_holes (init_board number))
        ~printer:string_of_int_pair_list

(** [marble_in_hole_test name coord players expected_output] constructs a OUnit
    test named [name] that asserts the equality of [expected_output] with
    [marble_in_hole coord players]. *)
let marble_in_hole_test (name : string) (coord : int * int) (players : int)
    (expected_output : m option) =
  name >:: fun _ ->
  assert_equal expected_output
    (marble_in_hole (init_board players) coord)
    ~printer:string_of_marble_option

(** [coord_of_marble_test name b m expected_output] constructs a OUnit test
    named [name] that asserts the equality of [expected_output] with
    [coord_of_marble b m]. *)
let coord_of_marble_test (name : string) (b : Board.t) (m : Board.m)
    (expected_output : int * int) =
  name >:: fun _ ->
  assert_equal expected_output (coord_of_marble b m) ~printer:string_of_coord

(** [all_in_corner_test name b corner_c marble_c expected_output] constructs a
    OUnit test named [name] that asserts the equality of [expected_output] with
    [all_in_corner b corner_c marble_c]. *)
let all_in_corner_test (name : string) (b : Board.t) (corner_c : string)
    (marble_c : string) (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (all_in_corner b corner_c marble_c)
    ~printer:string_of_bool

(** [parse_test a expected_output] constructs a OUnit test named [name] that
    asserts the equality of [expected_output] with [parse a]. *)
let parse_test (name : string) (a : string) (expected_output : action) =
  name >:: fun _ ->
  assert_equal expected_output (parse a) ~printer:string_of_action

(** [current_board_test name st expected_output] constructs a OUnit test named
    [name] that asserts the equality of [expected_output] with
    [current_board st]. *)
let current_board_test (name : string) (st : State.t)
    (expected_output : Board.t) =
  name >:: fun _ ->
  assert_equal expected_output (current_board st) ~printer:string_of_board

(** [current_player_test name st expected_output] constructs a OUnit test named
    [name] that asserts the equality of [expected_output] with
    [current_player_test st]. *)
let current_player_test (name : string) (st : State.t)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (current_player st) ~printer:Fun.id

(** [num_players_test name st expected_output] constructs a OUnit test named
    [name] that asserts the equality of [expected_output] with [num_players st]. *)
let num_players_test (name : string) (st : State.t) (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (num_players st) ~printer:string_of_int

(** [last_marble_test name st expected_output] constructs a OUnit test named
    [name] that asserts the equality of [Some expected_output] with
    [Some (last_marble st)]. *)
let last_marble_test (name : string) (st : State.t) (expected_output : m) =
  name >:: fun _ ->
  assert_equal (Some expected_output)
    (Some (last_marble st))
    ~printer:string_of_marble_option

(** [move_test name m dir st expected_output] constructs a OUnit test named
    [name] that asserts the equality of [expected_output] with [move m dir st]. *)
let move_test (name : string) (m : int) (dir : string) (st : State.t)
    (expected_output : State.result) =
  name >:: fun _ ->
  assert_equal expected_output (move m dir st) ~printer:string_of_result

(** [marble_after_move_test name st coord expected_output] constructs a OUnit
    test named [name] that asserts that the marble at the coordinate [coord] on
    the board of [st] is [m]. *)
let marble_after_move_test (name : string) (st : State.t) (coord : int * int)
    (expected_output : m option) =
  name >:: fun _ ->
  assert_equal expected_output
    (marble_in_hole (current_board st) coord)
    ~printer:string_of_marble_option

(*************************************************************************)
(* BOARD TESTS *)
(*************************************************************************)

let init_marble_tests =
  List.map
    (fun test_record -> init_marbles_test test_record)
    init_marble_test_records
  @ List.map (fun t -> init_empty_holes_test t) init_empty_holes_records

let marble_in_hole_tests =
  [
    marble_in_hole_test "(13,17) is red1" (13, 17) 2
      (Some { color = "red"; number = 1 });
    marble_in_hole_test "(10,4) is black7 if 2 players" (10, 4) 2
      (Some { color = "black"; number = 7 });
    marble_in_hole_test "(10,4) is None if 1 player" (10, 4) 1 None;
    marble_in_hole_test "(13, 7)" (13, 7) 1 None;
    ( "invalid coord for marble_in_hole" >:: fun _ ->
      assert_raises
        (BadCoord (1, 1))
        (fun () -> marble_in_hole (init_board 2) (1, 1)) );
  ]

let coord_of_marble_tests =
  [
    coord_of_marble_test "red1 is (13,17)" (init_board 2)
      { color = "red"; number = 1 }
      (13, 17);
    ( "coord of invalid marble: <1" >:: fun _ ->
      assert_raises BadMarble (fun () ->
          coord_of_marble (init_board 2) { color = "red"; number = 0 }) );
    ( "coord of invalid marble: >10" >:: fun _ ->
      assert_raises BadMarble (fun () ->
          coord_of_marble (init_board 2) { color = "red"; number = 11 }) );
    ( "coord of invalid marble: bad color" >:: fun _ ->
      assert_raises BadMarble (fun () ->
          coord_of_marble (init_board 2) { color = "pink"; number = 4 }) );
  ]

let all_in_corner_tests =
  [
    all_in_corner_test "all marbles in red corner are red" (init_board 2) "red"
      "red" true;
    all_in_corner_test "all marbles in red corner are not black" (init_board 2)
      "red" "black" false;
    ( "invalid corner color: pink" >:: fun _ ->
      assert_raises BadMarble (fun () ->
          all_in_corner (init_board 2) "pink" "red") );
  ]

let board_tests =
  List.flatten
    [
      init_marble_tests;
      marble_in_hole_tests;
      coord_of_marble_tests;
      all_in_corner_tests;
    ]

(*************************************************************************)
(* ACTION TESTS *)
(*************************************************************************)

let empty_action_tests =
  [
    ( "invalid action: empty string" >:: fun _ ->
      assert_raises Empty (fun () -> parse "") );
    ( "invalid action: only spaces" >:: fun _ ->
      assert_raises Empty (fun () -> parse "                ") );
  ]

let end_tests =
  [
    parse_test "valid end action: end" "end" End;
    parse_test "valid end action: END" "END" End;
    parse_test "valid end action: End" "End" End;
    parse_test "valid end action: eNd" "eNd" End;
    ( "invalid end action" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "end 10") );
  ]

let quit_tests =
  [
    parse_test "valid quit action: quit" "quit" Quit;
    parse_test "valid quit action: QUIT" "QUIT" Quit;
    parse_test "valid quit action: Quit" "Quit" Quit;
    parse_test "valid quit action: qUiT" "qUiT" Quit;
    ( "invalid quit action" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "quit game") );
    parse_test "valid quit action: QuIt" "QuIt" Quit;
    ( "invalid quit action" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "quit game") );
    parse_test "valid quit action: QuIT" "QuIT" Quit;
    ( "invalid quit action" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "quit game") );
  ]

let invalid_move_tests =
  [
    ( "invalid move action: no number and direction" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "move") );
    ( "invalid move action: float as marble number" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "move 1.0 LU") );
    ( "invalid move action: float as marble number and invalid move action"
    >:: fun _ -> assert_raises Invalid_Action (fun () -> parse "move 112.0 LU")
    );
    ( "invalid move action: word as marble number" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "move one LU") );
    ( "invalid move action: marble number < 1" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "move 0 LU") );
    ( "invalid move action: marble number < 1" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "move -5 LU") );
    ( "invalid move action: marble number > 10" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "move 11 LU") );
    ( "invalid move action: not a direction code" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "move 5 up") );
    ( "invalid move action: not a direction code" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "move 20 down") );
    ( "invalid move action: not a direction code" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "move 5 sideways") );
    ( "invalid move action: not a direction code" >:: fun _ ->
      assert_raises Invalid_Action (fun () ->
          parse "move 5 to the left and sideways") );
  ]

let valid_move_tests =
  [
    (* case-insensitive test for move keyword *)
    parse_test "valid move action: move 5 L" "move 5     L" (Move (5, "L"));
    parse_test "valid move action: MOVE 5 L" "MOVE     5 L" (Move (5, "L"));
    parse_test "valid move action: Move 5 L" "  Move 5 L" (Move (5, "L"));
    parse_test "valid move action: mOvE 5 L" "mOvE 5 L       " (Move (5, "L"));
    parse_test "valid move action: mOvE 5 L" "mOvE 3 R       " (Move (3, "R"));
    (* case-insensitive tests for shortened directions *)
    parse_test "valid move action: move 5 l" "move 5 l" (Move (5, "L"));
    parse_test "valid move action: move 5 lu" "move 5 lu" (Move (5, "LU"));
    parse_test "valid move action: move 5 LU" "move 5 LU" (Move (5, "LU"));
    parse_test "valid move action: move 5 Lu" "move 5 Lu" (Move (5, "LU"));
    parse_test "valid move action: move 5 ld" "move 5 ld" (Move (5, "LD"));
    parse_test "valid move action: move 5 LD" "move 5 LD" (Move (5, "LD"));
    parse_test "valid move action: move 5 Ld" "move 5 Ld" (Move (5, "LD"));
    parse_test "valid move action: move 5 r" "move 5 r" (Move (5, "R"));
    parse_test "valid move action: move 5 ru" "move 5 ru" (Move (5, "RU"));
    parse_test "valid move action: move 5 RU" "move 5 RU" (Move (5, "RU"));
    parse_test "valid move action: move 5 Ru" "move 5 Ru" (Move (5, "RU"));
    parse_test "valid move action: move 5 rd" "move 5 rd" (Move (5, "RD"));
    parse_test "valid move action: move 5 Rd" "move 5 Rd" (Move (5, "RD"));
    parse_test "valid move action: move 2 lu" "move 2 lu" (Move (2, "LU"));
    parse_test "valid move action: move 2 lU" "move 2 lU" (Move (2, "LU"));
    parse_test "valid move action: move 1 lu" "move 1 l" (Move (1, "L"));
    (* case-insensitive tests for long directions *)
    parse_test "valid move action: move 5 left" "move 5 left" (Move (5, "L"));
    parse_test "valid move action: move 5 LEFT" "move 5 LEFT" (Move (5, "L"));
    parse_test "valid move action: move 5 left-up" "move 5 left-up"
      (Move (5, "LU"));
    parse_test "valid move action: move 5 Left-Up" "move 5 Left-Up"
      (Move (5, "LU"));
    parse_test "valid move action: move 5 left-down" "move 5 left-down"
      (Move (5, "LD"));
    parse_test "valid move action: move 5 lEFT-dOWN" "move 5 lEFT-dOWN"
      (Move (5, "LD"));
    parse_test "valid move action: move 5 lEfT-DOwN" "move 5 lEfT-DOwN"
      (Move (5, "LD"));
    parse_test "valid move action: move 5 right" "move 5 right" (Move (5, "R"));
    parse_test "valid move action: move 5 Right" "move 5 Right" (Move (5, "R"));
    parse_test "valid move action: move 5 riGHt" "move 5 riGHt" (Move (5, "R"));
    parse_test "valid move action: move 5 right-up" "move 5 right-up"
      (Move (5, "RU"));
    parse_test "valid move action: move 5 rIgHt-Up" "move 5 rIgHt-Up"
      (Move (5, "RU"));
    parse_test "valid move action: move 5 right-down" "move 5 right-down"
      (Move (5, "RD"));
    parse_test "valid move action: move 10 right-down" "move 10 right-down"
      (Move (10, "RD"));
    parse_test "valid move action: move 10 riGht-doWn" "move 10 right-down"
      (Move (10, "RD"));
  ]

let invalid_first_word_tests =
  [
    ( "invalid first word: hello" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "hello") );
    ( "invalid first word: skip" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "skip") );
    ( "invalid first word: moev 10 LU" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "moev 10 LU") );
    ( "invalid first word: mevo 10 LU" >:: fun _ ->
      assert_raises Invalid_Action (fun () -> parse "mevo 10 LU") );
  ]

let action_tests =
  List.flatten
    [
      empty_action_tests;
      end_tests;
      quit_tests;
      invalid_move_tests;
      valid_move_tests;
      invalid_first_word_tests;
    ]

(*************************************************************************)
(* STATE TESTS *)
(*************************************************************************)

(* Defining some states to test board setup and movements *)
let initial_state_2 = init_state (init_board 2) 2 [ 0; 0; 0; 0; 0; 0 ] true
let initial_state_3 = init_state (init_board 3) 3 [ 0; 0; 0; 0; 0; 0 ] true
let initial_state_4 = init_state (init_board 4) 4 [ 0; 0; 0; 0; 0; 0 ] true
let initial_state_5 = init_state (init_board 5) 5 [ 0; 0; 0; 0; 0; 0 ] true
let initial_state_6 = init_state (init_board 6) 6 [ 0; 0; 0; 0; 0; 0 ] true

let initial_state_6_nohop =
  init_state (init_board 6) 6 [ 0; 0; 0; 0; 0; 0 ] false

let black_turn_nohop = end_turn initial_state_6_nohop
let yellow_turn_nohop = end_turn black_turn_nohop
let blue_turn_nohop = end_turn yellow_turn_nohop
let white_turn_nohop = end_turn black_turn_nohop
let green_turn_nohop = end_turn white_turn_nohop

let current_board_tests =
  [
    current_board_test "2P initial current board" initial_state_2 (init_board 2);
    current_board_test "3P initial current board" initial_state_3 (init_board 3);
    current_board_test "4P initial current board" initial_state_4 (init_board 4);
    current_board_test "5P initial current board" initial_state_5 (init_board 5);
    current_board_test "6P initial current board" initial_state_6 (init_board 6);
  ]

let black_turn = end_turn initial_state_6
let yellow_turn = end_turn black_turn
let blue_turn = end_turn yellow_turn
let white_turn = end_turn blue_turn
let green_turn = end_turn white_turn

let current_player_tests =
  [
    current_player_test "6P initial current player: red" initial_state_6 "red";
    current_player_test "6P current player: black" black_turn "black";
    current_player_test "6P current player: yellow" yellow_turn "yellow";
    current_player_test "6P current player: blue" blue_turn "blue";
    current_player_test "6P current player: white" white_turn "white";
    current_player_test "6P current player: green" green_turn "green";
  ]

let num_players_tests =
  [
    num_players_test "2P num players: 2" initial_state_2 2;
    num_players_test "3P num players: 3" initial_state_3 3;
    num_players_test "4P num players: 4" initial_state_4 4;
    num_players_test "5P num players: 5" initial_state_5 5;
    num_players_test "6P num players: 6" initial_state_6 6;
  ]

let last_marble_tests = []
let invalid_marble_msg = "Pick a marble between 1 and 10!"

let invalid_marble_number_tests =
  [
    move_test "move: invalid marble number 11" 11 "L" yellow_turn
      (Illegal (yellow_turn, invalid_marble_msg));
    move_test "move: invalid marble number -1" ~-1 "L" blue_turn
      (Illegal (blue_turn, invalid_marble_msg));
    move_test "move: invalid marble number -1" ~-11 "L" blue_turn
      (Illegal (blue_turn, invalid_marble_msg));
    move_test "move: invalid marble number 100" 100 "L" white_turn
      (Illegal (white_turn, invalid_marble_msg));
    move_test "move: invalid marble number 0" 0 "L" white_turn
      (Illegal (white_turn, invalid_marble_msg));
    move_test "move: invalid marble number 150" 150 "L" white_turn
      (Illegal (white_turn, invalid_marble_msg));
  ]

let invalid_dest_msg =
  "You can't move off the board, into an occupied hole, or try to move to an \
   adjacent hole after doing one or more hops over marbles. Try again!"

let invalid_destination_slide_tests =
  [
    move_test "move: invalid slide R to occupied destination for red marble 7" 7
      "R" initial_state_2
      (Illegal (initial_state_2, invalid_dest_msg));
    move_test "move: invalid slide L to off-board destination for red marble 7"
      7 "L" initial_state_2
      (Illegal (initial_state_2, invalid_dest_msg));
    move_test
      "move: invalid slide LU to off-board destination for black marble 1" 1
      "LU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
    move_test
      "move: invalid slide RU to off-board destination for black marble 3" 3
      "RU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
    move_test
      "move: invalid slide LU to off-board destination for black marble 2" 2
      "LU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
    move_test
      "move: invalid slide LU to off-board destination for black marble 2" 2
      "LU" yellow_turn
      (Illegal (yellow_turn, invalid_dest_msg));
    move_test
      "move: invalid slide LU to off-board destination for yellow marble 1" 1
      "LU" yellow_turn
      (Illegal (yellow_turn, invalid_dest_msg));
    move_test
      "move: invalid slide LU to off-board destination for yellow marble 3" 3
      "LU" yellow_turn
      (Illegal (yellow_turn, invalid_dest_msg));
    move_test
      "move: invalid slide LU to off-board destination for yellow marble 4" 4
      "LU" yellow_turn
      (Illegal (yellow_turn, invalid_dest_msg));
    move_test
      "move: invalid slide LD to off-board destination for yellow marble 8" 8
      "LD" yellow_turn
      (Illegal (yellow_turn, invalid_dest_msg));
    move_test
      "move: invalid slide RU to off-board destination for blue marble 9" 9 "RU"
      blue_turn
      (Illegal (blue_turn, invalid_dest_msg));
    move_test
      "move: invalid slide RU to off-board destination for blue marble 10" 10
      "RU" blue_turn
      (Illegal (blue_turn, invalid_dest_msg));
    move_test
      "move: invalid slide RD to off-board destination for blue marble 4" 4 "RD"
      blue_turn
      (Illegal (blue_turn, invalid_dest_msg));
    move_test
      "move: invalid slide RD to off-board destination for blue marble 1" 1 "RD"
      blue_turn
      (Illegal (blue_turn, invalid_dest_msg));
    move_test
      "move: invalid slide LD to off-board destination for white marble 2" 2
      "LD" white_turn
      (Illegal (white_turn, invalid_dest_msg));
    move_test
      "move: invalid slide LD to off-board destination for white marble 4" 4
      "LD" white_turn
      (Illegal (white_turn, invalid_dest_msg));
    move_test
      "move: invalid slide LU to off-board destination for white marble 8" 8
      "LU" white_turn
      (Illegal (white_turn, invalid_dest_msg));
    move_test
      "move: invalid slide LU to off-board destination for white marble 5" 5
      "LU" white_turn
      (Illegal (white_turn, invalid_dest_msg));
    move_test
      "move: invalid slide RU to off-board destination for green marble 2" 2
      "RU" green_turn
      (Illegal (green_turn, invalid_dest_msg));
    move_test
      "move: invalid slide RD to off-board destination for green marble 7" 7
      "RD" green_turn
      (Illegal (green_turn, invalid_dest_msg));
  ]

let invalid_destination_hop_tests =
  [
    move_test "move: invalid hop RD to off-board destination for red marble 5" 5
      "RD" initial_state_2
      (Illegal (initial_state_2, invalid_dest_msg));
    move_test "move: invalid hop LD to off-board destination for red marble 5" 5
      "LD" initial_state_2
      (Illegal (initial_state_2, invalid_dest_msg));
    move_test "move: invalid hop RU to off-board destination for black marble 5"
      5 "RU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
    move_test "move: invalid hop LU to off-board destination for black marble 5"
      5 "LU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
    move_test "move: invalid hop LU to off-board destination for black marble 8"
      8 "LU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
    move_test "move: invalid hop RU to off-board destination for black marble 8"
      8 "RU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
    move_test "move: invalid hop LU to off-board destination for black marble 9"
      9 "LU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
    move_test "move: invalid hop LU to off-board destination for black marble 9"
      9 "RU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
    move_test
      "move: invalid hop LU to off-board destination for yellow marble 6" 6 "LU"
      yellow_turn
      (Illegal (yellow_turn, invalid_dest_msg));
    move_test
      "move: invalid hop LU to off-board destination for yellow marble 9" 9 "LU"
      yellow_turn
      (Illegal (yellow_turn, invalid_dest_msg));
    move_test
      "move: invalid hop LU to off-board destination for yellow marble 7" 7 "LU"
      yellow_turn
      (Illegal (yellow_turn, invalid_dest_msg));
    move_test
      "move: invalid hop LD to off-board destination for yellow marble 6" 6 "LD"
      yellow_turn
      (Illegal (yellow_turn, invalid_dest_msg));
    move_test
      "move: invalid hop LD to off-board destination for yellow marble 7" 7 "LD"
      yellow_turn
      (Illegal (yellow_turn, invalid_dest_msg));
    move_test "move: invalid hop RU to off-board destination for blue marble 6"
      6 "RU" blue_turn
      (Illegal (blue_turn, invalid_dest_msg));
    move_test "move: invalid hop RD to off-board destination for blue marble 6"
      6 "RD" blue_turn
      (Illegal (blue_turn, invalid_dest_msg));
    move_test "move: invalid hop RD to off-board destination for blue marble 9"
      9 "RD" blue_turn
      (Illegal (blue_turn, invalid_dest_msg));
    move_test "move: invalid hop RD to off-board destination for blue marble 10"
      10 "RD" blue_turn
      (Illegal (blue_turn, invalid_dest_msg));
    move_test "move: invalid hop RU to off-board destination for blue marble 2"
      2 "RU" blue_turn
      (Illegal (blue_turn, invalid_dest_msg));
    move_test "move: invalid hop RU to off-board destination for blue marble 1"
      1 "RU" blue_turn
      (Illegal (blue_turn, invalid_dest_msg));
    move_test "move: invalid hop LU to off-board destination for white marble 3"
      3 "LU" white_turn
      (Illegal (white_turn, invalid_dest_msg));
    move_test "move: invalid hop LU to off-board destination for white marble 4"
      4 "LU" white_turn
      (Illegal (white_turn, invalid_dest_msg));
    move_test "move: invalid hop RD to off-board destination for white marble 8"
      8 "RD" white_turn
      (Illegal (white_turn, invalid_dest_msg));
    move_test
      "move: invalid hop RD to off-board destination for white marble 10" 10
      "RD" white_turn
      (Illegal (white_turn, invalid_dest_msg));
    move_test "move: invalid hop RD to off-board destination for white marble 6"
      6 "RD" white_turn
      (Illegal (white_turn, invalid_dest_msg));
    move_test "move: invalid hop LU to off-board destination for white marble 6"
      6 "LU" white_turn
      (Illegal (white_turn, invalid_dest_msg));
    move_test "move: invalid hop RU to off-board destination for green marble 8"
      8 "RU" green_turn
      (Illegal (green_turn, invalid_dest_msg));
    move_test
      "move: invalid hop RU to off-board destination for green marble 10" 10
      "RU" green_turn
      (Illegal (green_turn, invalid_dest_msg));
    move_test "move: invalid hop RD to off-board destination for green marble 2"
      2 "RD" green_turn
      (Illegal (green_turn, invalid_dest_msg));
    move_test "move: invalid hop RD to off-board destination for green marble 6"
      6 "RD" green_turn
      (Illegal (green_turn, invalid_dest_msg));
  ]

let invalid_multihop_msg =
  "You can't move off the board, into an occupied hole, or try to move to an \
   adjacent hole after doing one or more hops over marbles. In hard mode, you \
   can only hop over one marble at a time, not over multiple marbles in a line \
   at once. Try again!"

let invalid_multihop_tests =
  [
    move_test "move: invalid multihop RU for red 2" 2 "RU" initial_state_6_nohop
      (Illegal (initial_state_6_nohop, invalid_multihop_msg));
    move_test "move: invalid multihop LU for red 1" 1 "LU" initial_state_6_nohop
      (Illegal (initial_state_6_nohop, invalid_multihop_msg));
    move_test "move: invalid multihop R for yellow 5" 5 "R" yellow_turn_nohop
      (Illegal (yellow_turn_nohop, invalid_multihop_msg));
    move_test "move: invalid mulithop L for green 4" 4 "L" green_turn_nohop
      (Illegal (green_turn_nohop, invalid_multihop_msg));
    move_test "move: invalid multihop RD for black 1" 1 "RD" black_turn_nohop
      (Illegal (black_turn_nohop, invalid_multihop_msg));
    move_test "move: Invalid multihop LD for black 1" 1 "LD" black_turn_nohop
      (Illegal (black_turn_nohop, invalid_multihop_msg));
  ]

(* Can't move 2 marbles in 1 turn, moving marbles*)
let invalid_2_marbles_msg = "You can only move one marble per turn!"
let move_red4_RU = state_of_result (move 4 "RU" initial_state_2)
let move_black6_RD = state_of_result (move 6 "RD" black_turn)
let move_yellow8_RD = state_of_result (move 8 "RD" yellow_turn)
let move_blue9_LU = state_of_result (move 9 "LU" blue_turn)
let move_white8_RU = state_of_result (move 8 "RU" white_turn)
let move_green9_LD = state_of_result (move 9 "LD" green_turn)

let invalid_slide_after_hop_test =
  [
    move_test "move: invalid slide after hop" 7 "LU" move_red4_RU
      (Illegal (move_red4_RU, invalid_2_marbles_msg));
    move_test "move: invalid slide after hop" 7 "LU" move_black6_RD
      (Illegal (move_black6_RD, invalid_2_marbles_msg));
    move_test "move: invalid slide after hop" 7 "LU" move_yellow8_RD
      (Illegal (move_yellow8_RD, invalid_2_marbles_msg));
    move_test "move: invalid slide after hop" 7 "LU" move_blue9_LU
      (Illegal (move_blue9_LU, invalid_2_marbles_msg));
    move_test "move: invalid slide after hop" 8 "R" move_green9_LD
      (Illegal (move_green9_LD, invalid_2_marbles_msg));
  ]

let invalid_moves_tests =
  List.flatten
    [
      invalid_marble_number_tests;
      invalid_destination_slide_tests;
      invalid_destination_hop_tests;
      invalid_slide_after_hop_test;
      invalid_multihop_tests;
    ]

(* Slide moves *)
let slide_red7_RU = state_of_result (move 7 "RU" initial_state_2)
let slide_red7_LU = state_of_result (move 7 "LU" initial_state_2)

let slide_red7_L =
  state_of_result (move 7 "L" (slide_red7_LU |> end_turn |> end_turn))

let slide_red7_R =
  state_of_result (move 7 "R" (slide_red7_LU |> end_turn |> end_turn))

let slide_red7_LD =
  state_of_result (move 7 "LD" (slide_red7_RU |> end_turn |> end_turn))

let move_red7_RD =
  state_of_result (move 7 "RD" (slide_red7_LU |> end_turn |> end_turn))

let valid_slide_moves_tests =
  [
    marble_after_move_test "red 7 slides RU to (11,13)" slide_red7_RU (11, 13)
      (Some { color = "red"; number = 7 });
    marble_after_move_test "red 7 slides LU to (9,13)" slide_red7_LU (9, 13)
      (Some { color = "red"; number = 7 });
    marble_after_move_test "red 7 slides L to (7,13)" slide_red7_L (7, 13)
      (Some { color = "red"; number = 7 });
    marble_after_move_test "red 7 slides R to (11,13)" slide_red7_R (11, 13)
      (Some { color = "red"; number = 7 });
    marble_after_move_test "red 7 slides LD to (10,14)" slide_red7_LD (10, 14)
      (Some { color = "red"; number = 7 });
    marble_after_move_test "red 7 slides RD to (10,14)" move_red7_RD (10, 14)
      (Some { color = "red"; number = 7 });
    marble_after_move_test "hole (10,14) is empty after red 7 slides"
      slide_red7_RU (10, 14) None;
  ]

(* Hop moves for red *)
let hop_red1_LU = state_of_result (move 1 "LU" initial_state_2)
let hop_red1_RU = state_of_result (move 1 "RU" initial_state_2)
let hop_red1_LD = state_of_result (move 1 "LD" hop_red1_RU)
let hop_red1_RD = state_of_result (move 1 "RD" hop_red1_LU)

let hop_red10_L =
  state_of_result (move 10 "L" (slide_red7_LU |> end_turn |> end_turn))

let hop_red10_R = state_of_result (move 10 "R" hop_red10_L)

let hop_red2_LU =
  state_of_result (move 2 "LU" (slide_red7_LU |> end_turn |> end_turn))

let hop_red2_RD = state_of_result (move 2 "RD" hop_red2_LU)

let hop_red3_RU =
  state_of_result (move 3 "RU" (hop_red10_L |> end_turn |> end_turn))

let hop_red3_LD = state_of_result (move 3 "LD" hop_red3_RU)

let hop_red9_L =
  state_of_result (move 9 "L" (slide_red7_LU |> end_turn |> end_turn))

let hop_red9_R = state_of_result (move 9 "R" hop_red9_L)

let valid_red_hop_moves_tests =
  [
    (* hop over multiple marbles *)
    marble_after_move_test "red 1 hops LU to (9,13)" hop_red1_LU (9, 13)
      (Some { color = "red"; number = 1 });
    marble_after_move_test "red 1 hops RU to (17,13)" hop_red1_RU (17, 13)
      (Some { color = "red"; number = 1 });
    marble_after_move_test "red 1 hops LD to (13,17)" hop_red1_LD (13, 17)
      (Some { color = "red"; number = 1 });
    marble_after_move_test "red 1 hops RD to (13,17)" hop_red1_RD (13, 17)
      (Some { color = "red"; number = 1 });
    marble_after_move_test "red 10 hops L to (10,14)" hop_red10_L (10, 14)
      (Some { color = "red"; number = 10 });
    marble_after_move_test "red 10 hops R to (16,14)" hop_red10_R (16, 14)
      (Some { color = "red"; number = 10 });
    (* hop over one marble *)
    marble_after_move_test "red 2 hops LU to (10,14)" hop_red2_LU (10, 14)
      (Some { color = "red"; number = 2 });
    marble_after_move_test "red 2 hops RD to (12,16)" hop_red2_RD (12, 16)
      (Some { color = "red"; number = 2 });
    marble_after_move_test "red 3 hops RU to (16,14)" hop_red3_RU (16, 14)
      (Some { color = "red"; number = 3 });
    marble_after_move_test "red 3 hops RD to (14,16)" hop_red3_LD (14, 16)
      (Some { color = "red"; number = 3 });
    marble_after_move_test "red 9 hops L to (10,14)" hop_red9_L (10, 14)
      (Some { color = "red"; number = 9 });
    marble_after_move_test "red 9 hops R to (14,14)" hop_red9_R (14, 14)
      (Some { color = "red"; number = 9 });
  ]

let hop_black1_RD = state_of_result (move 1 "RD" black_turn)
let hop_black1_LD = state_of_result (move 1 "LD" black_turn)
let slide_black7_LD = state_of_result (move 7 "LD" black_turn)
let slide_black7_RD = state_of_result (move 7 "RD" black_turn)

let invalid_dir_msg =
  "Choose a direction: L (left), R (right), LU (left-up), RU (right-up), LD \
   (left-down), RD (right-down)."

let black_moves_tests =
  [
    marble_after_move_test "black 1 hops RD to (17,5)" hop_black1_RD (17, 5)
      (Some { color = "black"; number = 1 });
    marble_after_move_test "black 1 hops LD to (9,5)" hop_black1_LD (9, 5)
      (Some { color = "black"; number = 1 });
    marble_after_move_test "black 7 slides LD to (9,5)" slide_black7_LD (9, 5)
      (Some { color = "black"; number = 7 });
    marble_after_move_test "black 7 slides RD to (11,5)" slide_black7_RD (11, 5)
      (Some { color = "black"; number = 7 });
    move_test "move: invalid marble number 0" 0 "R" black_turn
      (Illegal (black_turn, invalid_marble_msg));
    move_test "move: can't move black 1 D" 1 "D" black_turn
      (Illegal (black_turn, invalid_dir_msg));
    move_test "move: can't hop black 3 LU" 3 "LU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
    move_test "move: can't hop black 2 RU" 2 "RU" black_turn
      (Illegal (black_turn, invalid_dest_msg));
  ]

let hop_yellow1_R = state_of_result (move 1 "R" yellow_turn)
let hop_yellow1_RD = state_of_result (move 1 "RD" yellow_turn)

let yellow_moves_tests =
  [
    marble_after_move_test "yellow 1 hops R to (9,5)" hop_yellow1_R (9, 5)
      (Some { color = "yellow"; number = 1 });
    marble_after_move_test "yellow 1 hops RD to (5,9)" hop_yellow1_RD (5, 9)
      (Some { color = "yellow"; number = 1 });
  ]

let hop_blue4_L = state_of_result (move 4 "L" blue_turn)
let hop_blue4_LU = state_of_result (move 4 "LU" blue_turn)

let blue_moves_tests =
  [
    marble_after_move_test "blue 4 hops L to (17,13)" hop_blue4_L (17, 13)
      (Some { color = "blue"; number = 4 });
    marble_after_move_test "blue 4 hops LU to (21,9)" hop_blue4_LU (21, 9)
      (Some { color = "blue"; number = 4 });
  ]

let hop_white1_R = state_of_result (move 1 "R" white_turn)
let hop_white1_RU = state_of_result (move 1 "RU" white_turn)

let white_moves_tests =
  [
    marble_after_move_test "white 1 hops R to (9,13)" hop_white1_R (9, 13)
      (Some { color = "white"; number = 1 });
    marble_after_move_test "white 1 hops RU to (5,9)" hop_white1_RU (5, 9)
      (Some { color = "white"; number = 1 });
    move_test "move: can't move white 0 D" 0 "D" white_turn
      (Illegal (white_turn, invalid_marble_msg));
  ]

let hop_green4_L = state_of_result (move 4 "L" green_turn)
let hop_green4_LD = state_of_result (move 4 "LD" green_turn)

let green_moves_tests =
  [
    marble_after_move_test "green 4 hops L to (17,5)" hop_green4_L (17, 5)
      (Some { color = "green"; number = 4 });
    marble_after_move_test "green 4 hops LD to (21,9)" hop_green4_LD (21, 9)
      (Some { color = "green"; number = 4 });
    move_test "move: can't move green 4 U" 4 "U" green_turn
      (Illegal (green_turn, invalid_dir_msg));
  ]

let valid_hop_moves_tests = valid_red_hop_moves_tests

let valid_moves_tests =
  List.flatten [ valid_slide_moves_tests; valid_hop_moves_tests ]

let moves_tests =
  List.flatten
    [
      valid_move_tests;
      invalid_moves_tests;
      black_moves_tests;
      yellow_moves_tests;
      blue_moves_tests;
      white_moves_tests;
      green_moves_tests;
    ]

let state_tests =
  List.flatten
    [
      current_board_tests;
      current_player_tests;
      num_players_tests;
      last_marble_tests;
      moves_tests;
    ]

let suite =
  "test suite for final-project"
  >::: List.flatten [ board_tests; action_tests; state_tests ]

let _ = run_test_tt_main suite
