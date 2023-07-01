(* This partial grammar provides rules for lhs values. *)

%{
  let lfun lfun_obj lfun_args lfun_ret = Lfun { lfun_obj; lfun_args; lfun_ret }
%}

%%

(* lhs *
 *******
 * Handles lhs values.
 *
 * Returns an lhs
 *)
%public %inline simple_lhs:
  | lhs_id     { $1 }
  | tuple(lhs) { Ltuple $1 }
  | LITEXP     { Llitexpr $1 }


(* lhs id *
 **********
 * Handles lhs identifiers.
 *
 * Returns an lhs
 *)
%public %inline lhs_id: LID { Lsym $1 }


(* Function definition lhs *
 ***************************
 * Handles lhs functions definitions, i.e. an lhs followed by a parameter list.
 *
 * Returns an lhs
 *)
%public lhs_fun:
  | lhs_id parameter_list(some(tpar(lhs_id, True)))                     { lfun $1 $2 None }
  | lhs_id parameter_list(some(tpar(lhs_id, True))) ARROW lax(typespec) { lfun $1 $2 $4 }

%public %inline lhs:
  | tpar(simple_lhs, True)   { $1 }
  | tpar(lhs_fun, True)      { $1 }
  | LBRACK rhs(False) RBRACK { Lidx $2 }
  | LBRACK PLUS RBRACK       { Lmore }
  | SQUARE                   { Llist }
