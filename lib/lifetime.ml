(** This module handles symbol lifetimes *)


(** Symbol lifetime *)
type t = Between of int * int | From of int

(** Build a lifetime for a symbol to be woken at a specific moment
    @param i: When to wake the symbol *)
let at i = Between (i, i)

(** Build a lifetime for a symbol to be woken until a specific moment
    @param i: When to wake the symbol last *)
let until i = Between (0, i)

(** Build a lifetime for a symbol to be woken at the present moment *)
let now = at 0

let eternal = From 0

let from i = From i

(** Convert a lifetime into a string *)
let to_string = function
  | Between (0, 0) -> "now"
  | Between (i, j) when i = j -> "at " ^ string_of_int i
  | Between (i, j) -> "between " ^ string_of_int i ^ " and " ^ string_of_int j
  | From 0 -> "eternal"
  | From i -> "from " ^ string_of_int i
