open Game
open Board

(** [play_game p] starts the game with [p] players. *)
let play_game p =
  if p < 0 || p > 6 then print_endline "Please enter a number between 1 and 6."
  else
    let b = init_board p in
    print_endline (string_of_board b 1 1)

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