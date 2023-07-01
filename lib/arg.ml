type 'a t = Arg of 'a | Etc
let map f = function Arg a -> Arg (f a) | Etc -> Etc
let get = function Arg a -> a | Etc -> failwith "get"

let to_list = function Arg a -> [a] | Etc -> []
let fold ~etc ~arg = function Arg a -> arg a | Etc -> etc

type len = Exactly of int | More_than of int
let len_add len len' = match len, len' with
  | Exactly i  , Exactly j   -> Exactly (i+j)
  | More_than i, Exactly j
  | Exactly i  , More_than j
  | More_than i, More_than j -> More_than (i+j)

let rec length_of = function
  | []        -> Exactly 0
  | Arg _::tl -> len_add (Exactly 1) (length_of tl)
  | Etc::tl   -> len_add (More_than 0) (length_of tl)

let rec assoc args args' = match args, args' with
  | Arg a::tl, Arg a'::tl' -> (a, a')::assoc tl tl'
  | Arg _::_ , []
  | []       , Arg _::_    -> failwith "Argument mismatch"
  | []       , []          -> []
  | _                      -> failwith "Arg.assoc: not implemented"
(*
  | Etc::tl  , Arg _::_    -> List.rev (assoc (List.rev tl) (List.rev args')

let assoc' args dots args' dots' =
  let rec _assoc' acc = function
    | Arg a::tl, Arg a'::tl'  -> _assoc' (Some a, Some a')::acc tl tl'
    | Arg _::_ , [] when dots'
    | []       , Arg _::_ when dots ...
    | Arg _::_ , []
    | []       , Arg _::_     -> failwith "Argument mismatch"
    | []       , []           -> acc
*)
