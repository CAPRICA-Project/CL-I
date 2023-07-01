(* This partial grammar handles structures. It is not production-ready and not worth going through
   tbh :)
 *)

%{
  let call call_obj call_args = Call { call_obj; call_args }
  let tcall call_obj args_opt =
    let call_args = List.flatten (Option.to_list args_opt) in
    Tcall { call_obj; call_args }
  let flatten lo = List.flatten (Option.to_list lo)
(*
  let (!++) i = i := !i + 1; !i
*)
%}

%token COMPONENT "component"
%token INTERFACE "interface"
%token AT "@"
%token HOOK "^"
%token PRAGMA "::"

%%

%public structure:
  | component_structure { $1 }
  | pragma { $1 }

pragma: PRAGMA dot_rhs COLON { [def_val (Lsym "@@PRAGMA") $2] }

component_structure:
  | annotation* COMPONENT boption(INTERFACE) more_list(COMMA, csig) COLON
      { List.map (fun (ty, rty) ->
          let rty = { rty with cmp_ann = $1 } in
          def_ty ty (Cmp rty)) $4 }
  | annotation* COMPONENT boption(INTERFACE) csig component_body
      { let (ty, rty) = $4 in
        let (cmp_body, cmp_hooks) = $5 in
        let rty = { rty with cmp_body; cmp_ann = $1; cmp_hooks } in
        [def_ty ty (Cmp rty)] }

csig:
  | UID lhs_type_params? component_parameters?
      { let ty = Ltype { ty_name = $1; ty_params = $2; ty_sub = [] } in
        let rty = { cmp_args = flatten $3; cmp_body = None; cmp_ann = []; cmp_hooks = [] } in
        (ty, rty) }
  | LPAREN UID lhs_type_params? SUBTYPEOF separated_nonempty_list(COMMA, dot_rhs_ty) RPAREN component_parameters?
      { let ty = Ltype { ty_name = $2; ty_params = $3; ty_sub = $5 } in
        let rty = { cmp_args = flatten $7; cmp_body = None; cmp_ann = []; cmp_hooks = [] } in
        (ty, rty) }

component_parameters: inpar(separated_list(COMMA, arg(tpar(lhs_id, True)))) { $1 }

annotation: AT rhs_type { $2 }

hook: HOOK dot_rhs { $2 }

(* Component body *
 ******************
 * Handles component bodies.
 *
 * Returns a construct_body list option
 *)
component_body:
  | hook* COLON                       { (None, $1) }
  | EQUAL rhs(True)                   { (Some (CEq $2), []) }
  | EQUAL rhs(True) hook+ COLON       { (Some (CEq $2), $3) }
  | LBRACE structures_or_defs RBRACE  { (Some (CBody $2), []) }
  | LBRACE structures_or_defs RBRACE hook+ COLON { (Some (CBody $2), $4) }
