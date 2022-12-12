type action =
  | Move of (int * string)
  | End
  | Quit

exception Empty
exception Invalid_Action

(** [safe_to_string n] is either the int representation of [n] or -1 if [n] is
    not an int. *)
let safe_to_string n = try int_of_string n with Failure _ -> -1

(** [encode_direction d] is the direction [d] transformed into one of the six
    codes: L, LU, LD, R, RU, and RD, case-insensitive. If [d] isn't a valid
    direction, it is unchanged.

    Example: ru, Ru, right-up, RIGHT UP, all get encoded to RU. *)
let encode_direction d =
  match String.lowercase_ascii d with
  | "l" | "left" -> "L"
  | "lu" | "left-up" -> "LU"
  | "ld" | "left-down" -> "LD"
  | "r" | "right" -> "R"
  | "ru" | "right-up" -> "RU"
  | "rd" | "right-down" -> "RD"
  | _ -> d

(** [check_move lst] is the move [Move (n, d)]. Raises: Invalid_Action if the
    move isn't valid. *)
let check_move lst =
  match lst with
  | [ n; d ] ->
      let valid_n = safe_to_string n in
      let valid_d = encode_direction d in
      if
        valid_n > 0 && valid_n < 11
        && List.mem valid_d [ "L"; "LU"; "LD"; "R"; "RU"; "RD" ]
      then Move (valid_n, valid_d)
      else raise Invalid_Action
  | _ -> raise Invalid_Action

let parse s =
  let word_list = List.filter (fun a -> a <> "") (String.split_on_char ' ' s) in
  match word_list with
  | [] -> raise Empty
  | h :: t -> (
      match String.lowercase_ascii h with
      | "move" -> check_move t
      | "end" -> if t <> [] then raise Invalid_Action else End
      | "quit" -> if t <> [] then raise Invalid_Action else Quit
      | _ -> raise Invalid_Action)
