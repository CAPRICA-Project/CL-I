let solve_val = function
  | "=" | "!=" | "=>" | "&" | "|" | "!" | "<has" | "<can have" as s ->
      Some (Env.Constraint.of_untyped (Ext ("Z3\\" ^ s)))
  | "true" -> Some (Env.Constraint.of_typed (Boolean true) TBoolean)
  | "false" -> Some (Env.Constraint.of_typed (Boolean false) TBoolean)
  | _ -> None

let solve_ty = function
  | "Equivalence" | "Symmetric" | "Transitive" | "Reflexive" as s -> Some (Env.Constraint.of_type (Text ("Z3\\" ^ s)))
  | _ -> None
