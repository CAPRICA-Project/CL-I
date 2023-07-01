(* This partial grammar provides rules to type lhs values. *)

%{
  let lcons lcons_obj = function Some lcons_ty -> Lcons { lcons_obj; lcons_ty } | None -> lcons_obj
(*
  let lcons_adj lcons_obj adj_arg = lcons lcons_obj (Some (adj [] adj_arg))
*)
%}

%%

(* Typed lhs inside parentheses *
 ********************************
 * Handles lhs constraints between parentheses.
 *
 * Parameters:
 *   X (lhs): The lhs to constrain
 *   allow_paren (+bool): whether the expression can begin with an opening parenthesis
 *
 * Returns an lhs
 *)
%public %inline tpar(X, allow_paren): fun_in_par(t, X, allow_paren) { $1 }


(* Add a type constraint *
 *************************
 * Adds a type constraint to an lhs.
 *
 * Parameters:
 *   X (lhs): the lhs to constrain
 *
 * Returns an lhs
 *)
%public %inline t(X): X ioption(of_type) { lcons $1 $2 }


(* lhs function signature *
 **************************
 * Handles function signatures, i.e. functions without a body.
 *
 * Returns an lhs
 *)
%public %inline lhs_fun_sig:
  | lhs_id parameter_list(lax(typespec))                     { lcons $1 (Some (func [] $2 None)) }
  | lhs_id parameter_list(lax(typespec)) ARROW lax(typespec) { lcons $1 (Some (func [] $2 $4)) }

//%public lhs_sig: tpar(any3(simple_lhs, lhs_adj_sig, lhs_fun_sig), True) { $1 }
%public lhs_sig: tpar(simple_lhs, True) { $1 }
//%public lhs_sig: tpar(any2(simple_lhs, lhs_fun_sig), True) { $1 }
