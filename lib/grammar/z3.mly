%{
  open Ast

  let property z3_decl z3_body = { z3_decl; z3_body }
%}

%token FOR "for" ALL "all" EXISTS "exists"

%%

z3_parameters: inpar(separated_nonempty_list(COMMA, tpar(lhs_id, True))) { $1 }

%public z3_property:
  | FOR ALL z3_parameters LBRACE rhs(True)* RBRACE { Z3_forall (property $3 $5) }
  | EXISTS z3_parameters LBRACE rhs(True)* RBRACE { Z3_exists (property $2 $4) }
