type action =
  | Move of (int * string)
  | End
  | Quit

exception Empty
exception Invalid_Action

let check_move lst =
  match lst with
  | [ n; d ] ->
      let n = int_of_string n in
      if n > 0 && n < 11 && List.mem d [ "L"; "LU"; "LD"; "R"; "RU"; "RD" ] then
        Move (n, d)
      else raise Invalid_Action
  | _ -> raise Invalid_Action

let parse s =
  let word_list = List.filter (fun a -> a <> "") (String.split_on_char ' ' s) in
  match word_list with
  | [] -> raise Empty
  | h :: t -> (
      match h with
      | "move" -> check_move t
      | "end" -> if t <> [] then raise Invalid_Action else End
      | "quit" -> if t <> [] then raise Invalid_Action else Quit
      | _ -> raise Invalid_Action)
