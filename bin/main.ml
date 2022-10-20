open Game
open Board
open State
open Action

let rec run_game st =
  print_endline (string_of_board (current_board st));
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
