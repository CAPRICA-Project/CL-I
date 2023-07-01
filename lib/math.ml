let solve_val sym = match int_of_string_opt sym with
  | Some i -> Some (Env.Constraint.of_typed (Integer i) TInteger)
  | None   -> (match sym with
    | "+" | "/" | "*" | "#" | "=>" | "<" | ">" as s -> Some (Env.Constraint.of_untyped (Built_in ("Math\\"^s)))
    | _ -> None)

let add l = match List.hd l, List.hd (List.tl l) with
  | (_, Ast.Integer i, _, _), (_, Integer j, _, _)
      -> Ast.Integer (i+j)
  | (_, Unknown, _, _), _
  | _, (_, Unknown, _, _) -> Unknown
  | _ -> failwith "Math\\+: not implemented"

let div l = match List.hd l, List.hd (List.tl l) with
  | (_, Ast.Integer i, _, _), (_, Integer j, _, _)
      -> Ast.Integer (i/j)
  | (_, Unknown, _, _), _ -> Unknown
  | _ -> failwith "Math\\/: not implemented"

let repeat l n =
  let rec repeat_rec acc = function
    | 0 -> acc
    | n -> repeat_rec (List.rev_append l acc) (n-1) in
  List.rev (repeat_rec [] n)

let mul l = match List.hd l, List.hd (List.tl l) with
  | (_, Ast.Integer i, _, _), (_, Integer j, _, _)
      -> Ast.Integer (i*j)
  | (_, Rlist rl, _, _), (_, Integer j, _, _)
      -> Rlist (repeat rl j)
  | (_, Unknown, _, _), _ -> Unknown
  | _, (_, Unknown, _, _) -> Unknown
  | _ -> failwith "Math\\*: not implemented"

let card l = match List.hd l with
  | _, Ast.Rlist l, _, _ ->
      print_endline"XXX1";Ast.Integer (List.length l)
  | _, _, (Some (Ast.Rtlist { cardinality = Ast.From_to (Integer i, Integer j)::_; _}), []), _ when i = j ->
      print_endline"XXX2";Ast.Integer i
  | _, _, (Some (Ast.Rtlist _), _), _ ->
      print_endline"XXX3";Ast.Unknown
  | _, _, (None, []), _ -> print_endline"XXX4";Ast.Unknown
  | _ -> failwith "Math\\#: not implemented"

let implies _ = failwith "Math\\=>: not implemented"

let (vtable:string -> ('a*'b*'c*'d) list -> 'e)= function
  | "#" -> card
  | "+" -> add
  | "/" -> div
  | "*" -> mul
  | "=>" -> implies
  | _   -> failwith "??"
