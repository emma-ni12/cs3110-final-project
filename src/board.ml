open Yojson.Basic.Util

type m = {
  color : string;
  number : int;
}

type t = ((int * int) * m) list