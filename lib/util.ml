(*
let to_ast data = Compiler.compile (parse (Lexing.from_string data))
*)

let (|||) o o' = match o with
  | Some _ -> o
  | _      -> o'

let merge _ = (|||)

let[@tail_mod_cons] rec map3 f l l' l'' = match (l, l', l'') with
  | [], [], [] -> []
  | hd::[], hd'::[], hd''::[] -> [f hd hd' hd'']
  | hd1::hd2::tl, hd1'::hd2'::tl', hd1''::hd2''::tl'' ->
      let r = f hd1 hd1' hd1'' in
      let r' = f hd2 hd2' hd2'' in
      r::r'::map3 f tl tl' tl''
  | _ -> raise (Invalid_argument "map3")
