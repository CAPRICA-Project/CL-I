open Ast

let rec merge_cmp_bodies = function
  | [] -> failwith "merge_cmp_bodies: Empty list"
  | cmp_body::[] -> cmp_body
  | CBody l::CBody l'::tl -> merge_cmp_bodies (CBody (l@l')::tl)
  | _ -> failwith "merge_cmp_bodies: Not implemented"
