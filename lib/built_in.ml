let symbols = [%map.Env.Inherited.Attr Sym 0 => Env.Constraint.of_typed (Built_in "String\\starts_with") (Fun {fun_implicit = []; fun_args = [Arg (Some TString); Arg (Some TString)]; fun_ret = Some TBoolean}); Sym 1 => Env.Constraint.of_typed (Built_in "String\\ends_with") (Fun {fun_implicit = []; fun_args = [Arg (Some TString); Arg (Some TString)]; fun_ret = Some TBoolean}); Sym 2 => Env.Constraint.of_typed (Built_in "String\\contains") (Fun {fun_implicit = []; fun_args = [Arg (Some TString); Arg (Some TString)]; fun_ret = Some TBoolean})]

let solve_val = function
  | "assert" -> Some (Env.Constraint.of_typed
      (Built_in "assert")
      (Fun { fun_implicit = []; fun_args = [Arg (Some TBoolean)];
             fun_ret = Some Unit }))
  | "print" -> Some (Env.Constraint.of_typed
      (Built_in "print")
      (Fun { fun_implicit = []; fun_args = [Arg (Some TString)];
             fun_ret = Some Unit }))
  | "=" -> Some (Env.Constraint.of_typed
      (Built_in "=")
      (Fun { fun_implicit = []; fun_args = [Arg (Some Tunknown); Arg (Some Tunknown)];
             fun_ret = Some TBoolean }))
  | "!=" -> Some (Env.Constraint.of_typed
      (Built_in "!=")
      (Fun { fun_implicit = []; fun_args = [Arg (Some Tunknown); Arg (Some Tunknown)];
             fun_ret = Some TBoolean }))
  | "&" -> Some (Env.Constraint.of_typed
      (Built_in "&")
      (Fun { fun_implicit = [];
             fun_args = [Arg (Some TBoolean); Arg (Some TBoolean)];
             fun_ret = Some TBoolean }))
  | "true" -> Some (Env.Constraint.of_typed (Boolean true) TBoolean)
  | "false" -> Some (Env.Constraint.of_typed (Boolean false) TBoolean)
  | "<has" -> Some (Env.Constraint.of_untyped (Ext ("Z3\\<has"))) (*FIXME*)
  | "<can have" -> Some (Env.Constraint.of_untyped (Ext ("Z3\\<can have"))) (*FIXME*)
  | "!" -> Some (Env.Constraint.of_untyped (Ext ("Z3\\!"))) (*FIXME*)
  | _ -> None

let solve_ty = function
  | "Any"     -> Some (Env.Constraint.of_type (Text "Any"))
  | "Boolean" -> Some (Env.Constraint.of_type TBoolean)
  | "Integer" -> Some (Env.Constraint.of_type TInteger)
  | "String"  -> Some ([%map "starts_with" => Env.Inherited.Attr.Sym 0; "ends_with" => Env.Inherited.Attr.Sym 1; "contains" => Env.Inherited.Attr.Sym 2], None, (Some TString, []), None)
  | _         -> None

let eq l = match List.hd l, List.hd (List.tl l) with
  | (_, Ast.Ext i, _, _), (_, Ext j, _, _) -> Ast.Boolean (i = j)
  | (_, Built_in i, _, _), (_, Built_in j, _, _) -> Boolean (i = j)
  | (_, Boolean b, _, _), (_, Boolean b', _, _) -> Boolean (b = b')
  | (_, Unknown, _, _), _
  | _, (_, Unknown, _, _) -> Unknown
  | _ -> failwith "=: not implemented"

let neq l = match List.hd l, List.hd (List.tl l) with
  | (_, Ast.Ext i, _, _), (_, Ext j, _, _) -> Ast.Boolean (i != j)
  | (_, Built_in i, _, _), (_, Built_in j, _, _) -> Boolean (i != j)
  | (_, Boolean b, _, _), (_, Boolean b', _, _) -> Boolean (b != b')
  | (_, Unknown, _, _), _
  | _, (_, Unknown, _, _) -> Unknown
  | _ -> failwith "!=: not implemented"

let assert_ l = match List.hd l with
  | (_, Ast.Boolean true, _, _) -> Ast.Runit
  | (_, Ast.Boolean false, _, _) -> failwith "Assertion failed"
  | (_, Unknown, _, _) -> Unknown
  | _ -> failwith "assert: not implemented"

let and_ l = match List.hd l, List.hd (List.tl l) with
  | (_, Ast.Boolean b, _, _), (_, Ast.Boolean b', _, _) -> Ast.Boolean (b&&b')
  | (_, Unknown, _, _), _
  | _, (_, Unknown, _, _) -> Unknown
  | _ -> failwith "&: not implemented"

let vtable = function
  | "=" -> eq
  | "!=" -> neq
  | "assert" -> assert_
  | "&" -> and_
  | _   -> failwith "??"
