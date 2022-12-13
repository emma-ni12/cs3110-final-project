open Game
open Board
open State
open Action

(** [string_of_number n] is the two-digit string representation of the number
    [n]. If [n] is less than 10, then a 0 is added in front. Example:
    [string_of_number 1] is "01". Requires: 0 <= n <= 99 *)
let string_of_number n =
  if n < 10 then "0" ^ string_of_int n else string_of_int n

(** [marble_style m] is the styling of marble with color [m] to the terminal. *)
let marble_style = function
  | "red" -> [ ANSITerminal.on_red; ANSITerminal.white ]
  | "black" -> [ ANSITerminal.on_black; ANSITerminal.white ]
  | "yellow" -> [ ANSITerminal.on_yellow; ANSITerminal.black ]
  | "blue" -> [ ANSITerminal.on_blue; ANSITerminal.white ]
  | "white" -> [ ANSITerminal.on_white; ANSITerminal.black ]
  | "green" -> [ ANSITerminal.on_green; ANSITerminal.black ]
  | _ -> []

(** [print_marble m] is the terminal output of marble [m]. *)
let print_marble m =
  print_string "(";
  ANSITerminal.print_string (marble_style m.color) (string_of_number m.number);
  print_string ")"

let rec print_board_helper b row col =
  match (row, col) with
  | 18, _ -> print_string ""
  | r, 26 ->
      print_string "\n \n";
      print_board_helper b (r + 1) 1
  | r, c ->
      (match marble_in_hole b (c, r) with
      | exception BadCoord _ -> print_string "    "
      | Some m -> print_marble m
      | None -> print_string "(  )");
      print_board_helper b r (c + 1)

(** [print_board b] is the terminal output of board [b]. *)
let print_board b = print_board_helper b 1 1

(** [print_winning_player c] prints a message stating that the player of color
    [c] has won the game. *)
let print_winning_player c : unit =
  print_endline
    ("\nPLAYER " ^ c
   ^ " HAS WON THE GAME! THANKS FOR PLAYING!\n Play another round? (Y/N): ")

(** [print_win_message p] prints a win message for the player [p]. *)
let print_win_message p =
  match p with
  | "red" -> print_winning_player "🔴 RED 🔴"
  | "black" -> print_winning_player "⚫ BLACK ⚫"
  | "yellow" -> print_winning_player "🟡 YELLOW 🟡"
  | "blue" -> print_winning_player "🔵 BLUE 🔵"
  | "white" -> print_winning_player "⚪ WHITE ⚪"
  | "green" -> print_winning_player "🟢 GREEN 🟢"
  | _ -> failwith "Invalid color"

let print_score_helper s color =
  print_string ("|  " ^ color ^ ": " ^ string_of_int s ^ "  |")

let print_scoreboard st =
  let p = num_players st in
  match scoreboard st with
  | [ red; black; yellow; blue; white; green ] ->
      print_string "|";
      print_score_helper red "Red";
      print_score_helper black "Black";
      if p > 2 then print_score_helper yellow "Yellow";
      if p > 3 then print_score_helper blue "Blue";
      if p > 4 then print_score_helper white "White";
      if p > 5 then print_score_helper green "Green";
      print_string "|";
      print_newline ();
      print_newline ()
  | _ -> failwith "Incorrect state- scoreboard does not have 6 scores"

let rec play_again st =
  match read_line () with
  | "Y" | "y" | "Yes" | "yes" -> (
      match scoreboard st with
      | [ red; black; yellow; blue; white; green ] -> (
          match current_player st with
          | "red" ->
              let new_st =
                init_state
                  (init_board (num_players st))
                  (num_players st)
                  [ red + 1; black; yellow; blue; white; green ]
                  (multihop st)
              in
              run_game "" new_st
          | "black" ->
              let new_st =
                init_state
                  (init_board (num_players st))
                  (num_players st)
                  [ red; black + 1; yellow; blue; white; green ]
                  (multihop st)
              in
              run_game "" new_st
          | "yellow" ->
              let new_st =
                init_state
                  (init_board (num_players st))
                  (num_players st)
                  [ red; black; yellow + 1; blue; white; green ]
                  (multihop st)
              in
              run_game "" new_st
          | "blue" ->
              let new_st =
                init_state
                  (init_board (num_players st))
                  (num_players st)
                  [ red; black; yellow; blue + 1; white; green ]
                  (multihop st)
              in
              run_game "" new_st
          | "white" ->
              let new_st =
                init_state
                  (init_board (num_players st))
                  (num_players st)
                  [ red; black; yellow; blue; white + 1; green ]
                  (multihop st)
              in
              run_game "" new_st
          | "green" ->
              let new_st =
                init_state
                  (init_board (num_players st))
                  (num_players st)
                  [ red; black; yellow; blue; white; green + 1 ]
                  (multihop st)
              in
              run_game "" new_st
          | _ -> failwith "impossible to have this player color")
      | _ -> failwith "Incorrect state- scoreboard does not have 6 scores")
  | "N" | "n" | "No" | "no" ->
      print_endline "Hope you enjoyed our Chinese (Caml) Checkers!";
      exit 0
  | _ ->
      print_endline "Please type Y or N";
      play_again st

and run_game (message : string) st =
  print_scoreboard st;
  print_board (current_board st);
  print_endline message;
  print_endline
    (current_player st ^ "'s turn: 'move' a marble, 'end' turn, or 'quit' game.");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | action -> (
      match parse action with
      | (exception Invalid_Action) | (exception Empty) ->
          run_game
            "Invalid action, try again! ('move [number 1-10] [L, LU, LD, R, \
             RU, RD]' or 'end' or 'quit')"
            st
      | Move (n, d) -> (
          let new_st_result = move n d st in
          match new_st_result with
          | Legal (new_st, auto_end, game_won) ->
              if game_won then (
                print_board (current_board new_st);
                print_win_message (current_player new_st);
                play_again new_st)
              else if auto_end then
                let end_st = end_turn new_st in
                run_game
                  ("Moving marble " ^ string_of_int n ^ " to " ^ d
                 ^ ". Ended turn for " ^ current_player new_st)
                  end_st
              else
                run_game
                  ("Moving marble " ^ string_of_int n ^ " to " ^ d)
                  new_st
          | Illegal (st, message) -> run_game message st)
      | End ->
          let new_st = end_turn st in
          run_game ("Ended turn for " ^ current_player st) new_st
      | Quit ->
          print_endline "Quitting the game. Thanks for playing!";
          exit 0)

(** [play_game p] starts the game with [p] players if [p] is between 2-6.
    Otherwise, it loops until a valid input is received

    [invalid_players message] prints [message] to the terminal if the player
    doesn't enter a valid input for the number of players. *)
let rec play_game input hop_rule =
  try
    let p = int_of_string input in
    if p <= 1 || p > 6 then
      invalid_players
        "\nInvalid number of players. Please enter a number between 2 and 6!"
        hop_rule
    else
      let st = init_state (init_board p) p [ 0; 0; 0; 0; 0; 0 ] hop_rule in
      run_game "" st
  with Failure s ->
    invalid_players
      "\nSorry, that's not a number between 2 and 6. Please try again!" hop_rule

and invalid_players message hop_rule =
  print_endline message;
  print_string "> ";
  play_game (read_line ()) hop_rule

let data_dir_prefix = "data" ^ Filename.dir_sep

let rec print_hop_rule () =
  print_endline
    "\n\
     Play in easy mode (allows hopping over multiple marbles in a line) or \n\
     hard mode (only hop over a single marble at a time)? easy/hard: ";
  print_string "> ";
  match read_line () with
  | "easy" -> true
  | "hard" -> false
  | _ ->
      print_endline "Please type either \"easy\" or \"hard\".";
      print_hop_rule ()

(** [print_from_file filename] prints the contents of [filename] to the
    terminal.

    Reference: http://www.codecodex.com/wiki/Reading_text_from_a_file#OCaml *)
let print_from_file filename =
  let ch = open_in filename in
  try
    while true do
      print_endline (input_line ch)
    done
  with End_of_file -> close_in ch

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Chinese Caml 🐫 Checkers.\n";
  print_from_file "./data/instructions.txt";
  let hop_rule = print_hop_rule () in
  print_endline "\nPlease enter the number of players in the game. \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | players -> play_game players hop_rule

(* Execute the game engine. *)
let () = main ()
