open Env.Inherited.Attr

let process symbols =
  let v = object (self)
    inherit [_] Ast.reduce
    method zero = []
    method plus = List.rev_append
    method! visit_Ref _ i = [Sym i]
    method! visit_Lref _ i = [Sym i]
    method! visit_Lsome_of _ i = [Tsym i]
    method! visit_Tref _ i = [Tsym i]
    method! visit_Type_of _ i = [Sym i]
    method visit_tcons (_, ro, (ty, tys), _) =
      self#plus (self#plus (self#visit_option self#visit_rhs () ro)
                           (self#visit_option self#visit_rhs_ty () ty))
                (self#visit_list self#visit_rhs_ty () tys)
  end in
  let graph = Env.Sym_table.fold (fun sym tcons acc -> (sym, v#visit_tcons tcons)::acc) symbols [] in
  match Tsort.sort graph with
  | Sorted l -> l
  | ErrorCycle l -> print_endline (List.map to_string l |> String.concat "; "); failwith "Topo: Internal error"
