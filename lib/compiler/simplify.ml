open Ast

let run ast ref_sym =
  let v = object (self)
    inherit [_] mapreduce as super
    inherit [_] VisitorsRuntime.addition_monoid

    method! visit_rhs env = function
      | Sym_def (i, _(*child*)) when not (Symset.mem i ref_sym) -> failwith "DISABLED" (*self#visit_rhs env child*)
      | node                                               -> super#visit_rhs env node

    method visit_ast = self#visit_list self#visit_struct_
  end in
  let (ast, _) = v#visit_ast () ast in ast
