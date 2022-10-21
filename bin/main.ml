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

let rec run_game st =
  print_board (current_board st);
  print_endline
    (current_player st ^ "'s turn: 'move' a marble, 'end' turn, or 'quit' game.");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | action -> (
      match parse action with
      | (exception Invalid_Action) | (exception Empty) ->
          print_endline
            "Invalid action, try again! ('move [number 1-10] [L, LU, LD, R, \
             RU, RD]' or 'end' or 'quit')";
          run_game st
      | Move (n, d) -> (
          let new_st_result = move n d st in
          match new_st_result with
          | Legal new_st ->
              print_endline ("Moving marble " ^ string_of_int n ^ " to " ^ d);
              run_game new_st
          | Illegal st ->
              print_endline "Illegal argument, try again!";
              run_game st)
      | End ->
          print_endline ("Ended turn for " ^ current_player st);
          let new_st = end_turn st in
          run_game new_st
      | Quit ->
          print_endline "Quitting the game. Thanks for playing!";
          exit 0)

(** [play_game p] starts the game with [p] players. *)
let play_game p =
  if p < 0 || p > 6 then
    print_endline
      "Invalid number of players. Please run make play again and enter a \
       number between 1 and 6."
  else
    let st = init_state (init_board p) p in
    run_game st

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Chinese Caml 🐫 Checkers .\n";
  print_endline "Please enter the number of players in the game. \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | players -> play_game (int_of_string players)

(* Execute the game engine. *)
let () = main ()
