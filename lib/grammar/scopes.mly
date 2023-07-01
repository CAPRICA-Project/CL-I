(* This partial grammar handles scope openers. *)

%{
  let def def_obj def_value = Def { def_obj; def_value }
  let def_type deftype_obj deftype_ty = Def_type { deftype_obj; deftype_ty }

  let decl_val sym = def sym None
  let def_val sym value = def sym (Some value)
  let decl_ty sym = def_type sym None
  let def_ty sym ty = def_type sym (Some ty)

  let rec make_lalt = function
    | Lsym s                        -> Lalt s
    | Lcons ({ lcons_obj; _ } as c) -> Lcons { c with lcons_obj = make_lalt lcons_obj }
    | _                             -> failwith "Internal_failure"
%}

%token LET "let" TYPE "type"
%token SET "<-"

%%


(* Scope *
 *********
 * Handles scopes openers.
 *
 * Returns a def list
 *)
%public def:
  | decl              { $1 }
  | assignment(False) { [$1] }


(* Assignment *
 **************
 * Handles assignments (lhs or lhs_ty made equal to rhs or rhs_ty).
 *
 * Parameter:
 *   is_litt (+bool): whether the parser is in a litteral expression
 *
 * Returns a def
 *)
%public assignment(is_litt):
  | LET SQUARE tpar(lhs_id, True) EQUAL rhs(True)    { def_val (make_lalt $3) $5 }
  | LET lhs EQUAL rhs(True)                       { def_val $2 $4 }
//  | LET lhs_fun EQUAL rhs(True)                      { def_val $2 $4 }
//  | LET lhs_fun full_stmt_block                               { def_val $2 $3 }
  | TYPE lhs_type EQUAL typespec ifnot_(is_litt, COLON)       { def_ty $2 $4 }


//assignment_lhs:
//  | LID    { Lsym $1 }
//  | SQUARE { Lidx }


(* Declaration *
 ***************
 * Handles groups of declarations.
 *
 * Returns a def list
 *)
%public decl:
  | LET SQUARE tpar(lhs_id, True) COLON   { [decl_val (make_lalt $3)] }
  | LET lhs_sig COLON                     { [decl_val $2] }
  | LET more_list(COMMA, lhs_sig) COLON   { List.map decl_val $2 }
  | TYPE lhs_type COLON                   { [decl_ty $2] }
  | TYPE more_list(COMMA, lhs_type) COLON { List.map decl_ty $2 }


%public %inline set: rhs_value SET rhs(True) { { set_obj = $1; set_value = $3 } }
