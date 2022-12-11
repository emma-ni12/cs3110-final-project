(**Test Plan:

   Given that our group created a game (Chinese Caml Checkers), our testing
   approach consisted of both extensive play/manual testing and OUnit testing
   (tests in this file) to make sure certain features worked the way we expected
   them to.

   More specifically, we tested the board (shape, valid/invalid holes), state,
   destination validity, and sequences of movements with OUnit testing and we
   tested our win condition, ability to move marbles, and seeing if the game
   directions showed up at the beginning of the game with manual testing. Since
   the board, state, movements, and destination validity all contains pieces of
   logic that are needed to play the game and methods that modify the game state
   accordingly, we decided that it would be helpful to test all of these
   functions with OUnit tests. For functionality like win condition and ability
   to move marbles around the board, we felt like they were either easier to
   test through play testing or they were already indirectly tested through
   other functions.

   For features that we tested with OUnit testing, we employed the concepts of
   glass-box and black-box testing. One group member would write a function and
   test cases based on their implementation of the function (glass-box) and
   another group member would then write tests based only on the specifications
   of the function defined in the mli (black-box). We did not use randomized
   testing because our game system operates on predictable movements based on a
   set of predefined rules. Additionally, we felt that play testing our game
   helped us improve better make implementation decisions and customize user
   experience to achieve a desired output in specific situations.

   We believe that our test suite demonstrates the correctness of our systems
   because every function listed within our mli files was tested. We used
   property-based tests and checked if the correct outputs based on
   specifications were returned to ensure that the functions listed modify the
   game state properly. We also used the Bisect tool on our test suite to make
   sure all our files have above 90% test coverage. We felt that this amount of
   code coverage was enough for us to feel confident about our test suites,
   since some cases that we didn't test are considered unreachable.*)

open OUnit2
open Game
open Board
open State

(** [string_of_int_pair_list lst] is the string representation of a list of
    int*int pairs *)
let rec string_of_int_pair_list = function
  | [] -> ""
  | (x, y) :: t ->
      "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ") , "
      ^ string_of_int_pair_list t

(**[string_of_marble_option m] is the string representation of type m *)
let string_of_marble_option = function
  | None -> "empty hole"
  | Some { color; number } -> color ^ string_of_int number

(**[string_of_coord c] is the string representation of a coordinate [c]. *)
let string_of_coord (x, y) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

(**[string_of_state st] is the string representation of a state [st]. *)
let string_of_state (st : State.t) =
  let n_p = num_players st in
  let current_p = current_player st in
  let last_m = last_marble st in
  "State: board, number of players: " ^ string_of_int n_p ^ ", current player: "
  ^ current_p ^ ", last marble: "
  ^ string_of_marble_option (Some last_m)

(**[string_of_result r] is the string representation of the result [r]. *)
let string_of_result = function
  | Illegal (t, message) -> "Illegal: old state, message: " ^ message
  | Legal (t, auto_end, game_won) ->
      "Legal: new state, auto end: " ^ string_of_bool auto_end ^ ", game won: "
      ^ string_of_bool game_won

(** [cmp_pair_lists lst1 lst2] compares two lists of pairs to see whether they
    are equivalent lists of pairs, regardless of order *)
let cmp_pair_lists lst1 lst2 = List.for_all (fun a -> List.mem a lst2) lst1

(* End printers and helpers **************************************************)

(**[init_marbles_test name p expected_output] constructs a OUnit test named
   [name] that asserts the equality of [expected_output] with
   [holes_with_marbles] for a board with p players.*)
let init_marbles_test (name : string) (p : int)
    (expected_output : (int * int) list) =
  name >:: fun _ ->
  assert_equal expected_output ~cmp:cmp_pair_lists
    (holes_with_marbles (init_board p))
    ~printer:string_of_int_pair_list

(**[init_empty_holes_test name p expected_output] constructs a OUnit test named
   [name] that asserts the equality of [expected_output] with [empty_holes] for
   a board with p players.*)
let init_empty_holes_test (name : string) (p : int)
    (expected_output : (int * int) list) =
  name >:: fun _ ->
  assert_equal expected_output ~cmp:cmp_pair_lists
    (empty_holes (init_board p))
    ~printer:string_of_int_pair_list

(**[marble_in_hole_test name coord players expected_output] constructs a OUnit
   test named [name] that asserts the equality of [expected_output] with
   [marble_in_hole coord players].*)
let marble_in_hole_test (name : string) (coord : int * int) (players : int)
    (expected_output : m option) =
  name >:: fun _ ->
  assert_equal expected_output
    (marble_in_hole (init_board players) coord)
    ~printer:string_of_marble_option

(**[coord_of_marble_test name b m expected_output] constructs a OUnit test named
   [name] that asserts the equality of [expected_output] with
   [coord_of_marble b m].*)
let coord_of_marble_test (name : string) (b : Board.t) (m : Board.m)
    (expected_output : int * int) =
  name >:: fun _ ->
  assert_equal expected_output (coord_of_marble b m) ~printer:string_of_coord

(**[all_in_corner_test name b corner_c marble_c] constructs a OUnit test named
   [name] that asserts the equality of [expected_output] with
   [all_in_corner b corner_c marble_c].*)
let all_in_corner_test (name : string) (b : Board.t) (corner_c : string)
    (marble_c : string) (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (all_in_corner b corner_c marble_c)
    ~printer:string_of_bool

let move_test (name : string) (m : int) (dir : string) (st : State.t)
    (expected_output : State.result) =
  name >:: fun _ ->
  assert_equal expected_output (move m dir st) ~printer:string_of_result

let init_marble_tests =
  [
    init_marbles_test "one player, explicit" 1
      [
        (13, 17);
        (12, 16);
        (14, 16);
        (11, 15);
        (13, 15);
        (15, 15);
        (10, 14);
        (12, 14);
        (14, 14);
        (16, 14);
      ];
    init_marbles_test "three players, explicit" 3
      [
        (13, 17);
        (12, 16);
        (14, 16);
        (11, 15);
        (13, 15);
        (15, 15);
        (10, 14);
        (12, 14);
        (14, 14);
        (16, 14);
        (13, 1);
        (12, 2);
        (14, 2);
        (11, 3);
        (13, 3);
        (15, 3);
        (10, 4);
        (12, 4);
        (14, 4);
        (16, 4);
        (1, 5);
        (3, 5);
        (5, 5);
        (7, 5);
        (2, 6);
        (4, 6);
        (6, 6);
        (3, 7);
        (5, 7);
        (4, 8);
      ];
    init_empty_holes_test "6 players empty holes => center" 6
      [
        (9, 5);
        (11, 5);
        (13, 5);
        (15, 5);
        (17, 5);
        (8, 6);
        (10, 6);
        (12, 6);
        (14, 6);
        (16, 6);
        (18, 6);
        (7, 7);
        (9, 7);
        (11, 7);
        (13, 7);
        (15, 7);
        (17, 7);
        (19, 7);
        (6, 8);
        (8, 8);
        (10, 8);
        (12, 8);
        (14, 8);
        (16, 8);
        (18, 8);
        (20, 8);
        (5, 9);
        (7, 9);
        (9, 9);
        (11, 9);
        (13, 9);
        (15, 9);
        (17, 9);
        (19, 9);
        (21, 9);
        (6, 10);
        (8, 10);
        (10, 10);
        (12, 10);
        (14, 10);
        (16, 10);
        (18, 10);
        (20, 10);
        (7, 11);
        (9, 11);
        (11, 11);
        (13, 11);
        (15, 11);
        (17, 11);
        (19, 11);
        (8, 12);
        (10, 12);
        (12, 12);
        (14, 12);
        (16, 12);
        (18, 12);
        (9, 13);
        (11, 13);
        (13, 13);
        (15, 13);
        (17, 13);
      ];
    init_empty_holes_test "5 players empty holes => center and green" 5
      [
        (9, 5);
        (11, 5);
        (13, 5);
        (15, 5);
        (17, 5);
        (8, 6);
        (10, 6);
        (12, 6);
        (14, 6);
        (16, 6);
        (18, 6);
        (7, 7);
        (9, 7);
        (11, 7);
        (13, 7);
        (15, 7);
        (17, 7);
        (19, 7);
        (6, 8);
        (8, 8);
        (10, 8);
        (12, 8);
        (14, 8);
        (16, 8);
        (18, 8);
        (20, 8);
        (5, 9);
        (7, 9);
        (9, 9);
        (11, 9);
        (13, 9);
        (15, 9);
        (17, 9);
        (19, 9);
        (21, 9);
        (6, 10);
        (8, 10);
        (10, 10);
        (12, 10);
        (14, 10);
        (16, 10);
        (18, 10);
        (20, 10);
        (19, 5);
        (21, 5);
        (23, 5);
        (25, 5);
        (20, 6);
        (22, 6);
        (24, 6);
        (21, 7);
        (23, 7);
        (22, 8);
        (7, 11);
        (9, 11);
        (11, 11);
        (13, 11);
        (15, 11);
        (17, 11);
        (19, 11);
        (8, 12);
        (10, 12);
        (12, 12);
        (14, 12);
        (16, 12);
        (18, 12);
        (9, 13);
        (11, 13);
        (13, 13);
        (15, 13);
        (17, 13);
      ];
  ]

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
  ]

let board_tests =
  List.flatten
    [
      init_marble_tests;
      marble_in_hole_tests;
      coord_of_marble_tests;
      all_in_corner_tests;
    ]

let state_of_result result =
  match result with
  | Legal (st, _, _) -> st
  | Illegal (st, _) -> st

let state_tests =
  let initial_state_2 = init_state (init_board 2) 2 in
  let black_turn = end_turn initial_state_2 in
  let yellow_turn = end_turn (end_turn (init_state (init_board 6) 6)) in
  let blue_turn = end_turn yellow_turn in
  let white_turn = end_turn blue_turn in
  let green_turn = end_turn white_turn in
  let move_red7_RU = state_of_result (move 7 "RU" initial_state_2) in
  let move_red7_LU = state_of_result (move 7 "LU" initial_state_2) in
  let move_red7_R = state_of_result (move 7 "R" initial_state_2) in
  let move_red7_L = state_of_result (move 7 "L" initial_state_2) in
  let move_red1_LU = state_of_result (move 1 "LU" initial_state_2) in
  let move_red1_RD = state_of_result (move 1 "RD" move_red1_LU) in
  let move_red1_RU = state_of_result (move 1 "RU" initial_state_2) in
  let move_red1_LD = state_of_result (move 1 "LD" move_red1_RU) in
  let move_red9_L =
    state_of_result
      (move 9 "L"
         (end_turn (state_of_result (move 1 "LD" (end_turn move_red7_RU)))))
  in
  let move_red9_R = state_of_result (move 9 "R" move_red9_L) in
  let move_red0_RU = state_of_result (move 0 "RU" initial_state_2) in
  let move_red11_RU = state_of_result (move 11 "RU" initial_state_2) in
  let move_red4_RU = state_of_result (move 4 "RU" initial_state_2) in
  let move_red7_after_4 = state_of_result (move 7 "LU" move_red4_RU) in
  let move_red5_RD = state_of_result (move 5 "RD" initial_state_2) in
  let move_red5_LD = state_of_result (move 5 "LD" initial_state_2) in
  let move_black1_RD = state_of_result (move 1 "RD" black_turn) in
  let move_black1_LD = state_of_result (move 1 "LD" black_turn) in
  let move_black0_R = state_of_result (move 0 "R" black_turn) in
  let move_black1_D = state_of_result (move 1 "D" black_turn) in
  let move_black3_LU = state_of_result (move 3 "LU" black_turn) in
  let move_black2_RU = state_of_result (move 2 "RU" black_turn) in
  let move_black7_RD = state_of_result (move 7 "RD" black_turn) in
  let move_black7_LD = state_of_result (move 7 "LD" black_turn) in
  let move_yellow1_R = state_of_result (move 1 "R" yellow_turn) in
  let move_yellow1_RD = state_of_result (move 1 "RD" yellow_turn) in
  let move_yellow11_L = state_of_result (move 11 "L" yellow_turn) in
  let move_blue4_L = state_of_result (move 4 "L" blue_turn) in
  let move_blue4_LU = state_of_result (move 4 "LU" blue_turn) in
  let move_blueneg1_L = state_of_result (move ~-1 "L" blue_turn) in
  let move_white1_R = state_of_result (move 1 "R" white_turn) in
  let move_white1_RU = state_of_result (move 1 "RU" white_turn) in
  let move_white100_L = state_of_result (move 100 "L" white_turn) in
  let move_white0_D = state_of_result (move 0 "D" white_turn) in
  let move_green4_L = state_of_result (move 4 "L" green_turn) in
  let move_green4_LD = state_of_result (move 4 "LD" green_turn) in
  let move_green4_U = state_of_result (move 4 "U" green_turn) in

  [
    ( "2P initial current board" >:: fun _ ->
      assert_equal
        (string_of_board (init_board 2))
        (string_of_board (current_board initial_state_2))
        ~printer:Fun.id );
    ( "2P initial current player" >:: fun _ ->
      assert_equal "red" (current_player initial_state_2) );
    ( "2P initial number of players" >:: fun _ ->
      assert_equal 2 (num_players initial_state_2) );
    ( "red7 now in (11,13)" >:: fun _ ->
      assert_equal
        (Some { color = "red"; number = 7 })
        (marble_in_hole (current_board move_red7_RU) (11, 13))
        ~printer:string_of_marble_option );
    ( "red7 now in (9,13)" >:: fun _ ->
      assert_equal
        (Some { color = "red"; number = 7 })
        (marble_in_hole (current_board move_red7_LU) (9, 13))
        ~printer:string_of_marble_option );
    ( "red1 now in (9,13) hopping LU" >:: fun _ ->
      assert_equal
        (Some { color = "red"; number = 1 })
        (marble_in_hole (current_board move_red1_LU) (9, 13))
        ~printer:string_of_marble_option );
    ( "red1 now in (17,13) hopping RU" >:: fun _ ->
      assert_equal
        (Some { color = "red"; number = 1 })
        (marble_in_hole (current_board move_red1_RU) (17, 13))
        ~printer:string_of_marble_option );
    ( "red1 now in (13,17) hopping RD " >:: fun _ ->
      assert_equal
        (Some { color = "red"; number = 1 })
        (marble_in_hole (current_board move_red1_RD) (13, 17))
        ~printer:string_of_marble_option );
    ( "red1 now in (13,17) hopping LD " >:: fun _ ->
      assert_equal
        (Some { color = "red"; number = 1 })
        (marble_in_hole (current_board move_red1_LD) (13, 17))
        ~printer:string_of_marble_option );
    ( "red9 now in (10,14) hopping L" >:: fun _ ->
      assert_equal
        (Some { color = "red"; number = 9 })
        (marble_in_hole (current_board move_red9_L) (10, 14))
        ~printer:string_of_marble_option );
    ( "red9 now in (14,14) hopping R" >:: fun _ ->
      assert_equal
        (Some { color = "red"; number = 9 })
        (marble_in_hole (current_board move_red9_R) (14, 14))
        ~printer:string_of_marble_option );
    ( "black1 now in (17,5)" >:: fun _ ->
      assert_equal
        (Some { color = "black"; number = 1 })
        (marble_in_hole (current_board move_black1_RD) (17, 5))
        ~printer:string_of_marble_option );
    ( "black1 now in (9,5)" >:: fun _ ->
      assert_equal
        (Some { color = "black"; number = 1 })
        (marble_in_hole (current_board move_black1_LD) (9, 5))
        ~printer:string_of_marble_option );
    ( "black7 now in (9,5)" >:: fun _ ->
      assert_equal
        (Some { color = "black"; number = 7 })
        (marble_in_hole (current_board move_black7_LD) (9, 5))
        ~printer:string_of_marble_option );
    ( "black7 now in (11,5)" >:: fun _ ->
      assert_equal
        (Some { color = "black"; number = 7 })
        (marble_in_hole (current_board move_black7_RD) (11, 5))
        ~printer:string_of_marble_option );
    ( "yellow1 now in (9,5)" >:: fun _ ->
      assert_equal
        (Some { color = "yellow"; number = 1 })
        (marble_in_hole (current_board move_yellow1_R) (9, 5))
        ~printer:string_of_marble_option );
    ( "yellow1 now in (5,9)" >:: fun _ ->
      assert_equal
        (Some { color = "yellow"; number = 1 })
        (marble_in_hole (current_board move_yellow1_RD) (5, 9))
        ~printer:string_of_marble_option );
    ( "blue4 now in (17,13)" >:: fun _ ->
      assert_equal
        (Some { color = "blue"; number = 4 })
        (marble_in_hole (current_board move_blue4_L) (17, 13))
        ~printer:string_of_marble_option );
    ( "blue4 now in (21,9)" >:: fun _ ->
      assert_equal
        (Some { color = "blue"; number = 4 })
        (marble_in_hole (current_board move_blue4_LU) (21, 9))
        ~printer:string_of_marble_option );
    ( "white1 now in (9,13)" >:: fun _ ->
      assert_equal
        (Some { color = "white"; number = 1 })
        (marble_in_hole (current_board move_white1_R) (9, 13))
        ~printer:string_of_marble_option );
    ( "white1 now in (5,9)" >:: fun _ ->
      assert_equal
        (Some { color = "white"; number = 1 })
        (marble_in_hole (current_board move_white1_RU) (5, 9))
        ~printer:string_of_marble_option );
    ( "green4 now in (17,5)" >:: fun _ ->
      assert_equal
        (Some { color = "green"; number = 4 })
        (marble_in_hole (current_board move_green4_L) (17, 5))
        ~printer:string_of_marble_option );
    ( "green4 now in (21,9)" >:: fun _ ->
      assert_equal
        (Some { color = "green"; number = 4 })
        (marble_in_hole (current_board move_green4_LD) (21, 9))
        ~printer:string_of_marble_option );
    ( "(10, 14) is empty" >:: fun _ ->
      assert_equal None
        (marble_in_hole (current_board move_red7_RU) (10, 14))
        ~printer:string_of_marble_option );
    ( "2P initial can't move red 7 R" >:: fun _ ->
      assert_equal initial_state_2 move_red7_R ~printer:string_of_state );
    ( "2P initial can't move red 7 L" >:: fun _ ->
      assert_equal initial_state_2 move_red7_L ~printer:string_of_state );
    ( "2P initial can't move red 0 RU" >:: fun _ ->
      assert_equal initial_state_2 move_red0_RU ~printer:string_of_state );
    ( "2P initial can't move red 11 RU" >:: fun _ ->
      assert_equal initial_state_2 move_red11_RU ~printer:string_of_state );
    ( "2P initial can't hop red 5 RD" >:: fun _ ->
      assert_equal initial_state_2 move_red5_RD ~printer:string_of_state );
    ( "2P initial can't hop red 5 LD" >:: fun _ ->
      assert_equal initial_state_2 move_red5_LD ~printer:string_of_state );
    ( "2P initial can't move 2 marbles in 1 turn" >:: fun _ ->
      assert_equal move_red4_RU move_red7_after_4 ~printer:string_of_state );
    ( "2P initial can't move black 0 R" >:: fun _ ->
      assert_equal black_turn move_black0_R ~printer:string_of_state );
    ( "2P initial can't move black 1 D" >:: fun _ ->
      assert_equal black_turn move_black1_D ~printer:string_of_state );
    ( "2P initial can't hop black 3 LU" >:: fun _ ->
      assert_equal black_turn move_black3_LU ~printer:string_of_state );
    ( "2P initial can't hop black 2 RU" >:: fun _ ->
      assert_equal black_turn move_black2_RU ~printer:string_of_state );
    ( "2P initial can't move yellow 11 L" >:: fun _ ->
      assert_equal yellow_turn move_yellow11_L ~printer:string_of_state );
    ( "2P initial can't move blue -1 L" >:: fun _ ->
      assert_equal blue_turn move_blueneg1_L ~printer:string_of_state );
    ( "2P initial can't move white 100 L" >:: fun _ ->
      assert_equal white_turn move_white100_L ~printer:string_of_state );
    ( "2P initial can't move white 0 D" >:: fun _ ->
      assert_equal white_turn move_white0_D ~printer:string_of_state );
    ( "2P initial can't move green 4 U" >:: fun _ ->
      assert_equal green_turn move_green4_U ~printer:string_of_state );
  ]

let suite =
  "test suite for final-project" >::: List.flatten [ board_tests; state_tests ]

let _ = run_test_tt_main suite
