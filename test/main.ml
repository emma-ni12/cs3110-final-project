open OUnit2
open Game
open Board
open State

(** printers go here *)

(** [string_of_int_pair_list lst] is the string representation of a list of
    int*int pairs.*)
let rec string_of_int_pair_list = function
  | [] -> ""
  | (x, y) :: t ->
      "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ") , "
      ^ string_of_int_pair_list t

(**[string_of_marble_option m] is the string representation of type m*)
let string_of_marble_option = function
  | None -> "empty hole"
  | Some { color; number } -> color ^ string_of_int number

(** [cmp_pair_lists lst1 lst2] compares two lists of pairs to see whether they
    are equivalent lists of pairs, regardless of order. *)
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

let board_tests =
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
    marble_in_hole_test "(13,17) is red1" (13, 17) 1
      (Some { color = "red"; number = 1 });
    marble_in_hole_test "(10,4) is black7 if 2 players" (10, 4) 2
      (Some { color = "black"; number = 7 });
    marble_in_hole_test "(10,4) is None if 1 player" (10, 4) 1 None;
    marble_in_hole_test "(13, 7)" (13, 7) 1 None;
    ( "invalid coord for marble_in_hole" >:: fun _ ->
      assert_raises
        (BadCoord (1, 1))
        (fun () -> marble_in_hole (init_board 1) (1, 1)) );
  ]

let state_of_result result =
  match result with
  | Legal st -> st
  | Illegal st -> st

let state_tests =
  let initial_state_2 = init_state (init_board 2) 2 in
  let move_red7_RU = state_of_result (move 7 "RU" initial_state_2) in
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
    ( "(10, 14) is empty" >:: fun _ ->
      assert_equal None
        (marble_in_hole (current_board move_red7_RU) (10, 14))
        ~printer:string_of_marble_option );
    ( "print board" >:: fun _ ->
      assert_equal "hi"
        (string_of_board (current_board move_red7_RU))
        ~printer:Fun.id );
  ]

let suite =
  "test suite for final-project" >::: List.flatten [ board_tests; state_tests ]

let _ = run_test_tt_main suite
