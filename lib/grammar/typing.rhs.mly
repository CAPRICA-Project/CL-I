(* This partial grammar provides rules to type rhs values. *)

%{
(*
  let rcons rcons_obj rcons_tag rcons_ty = Rcons { rcons_obj; rcons_tag; rcons_ty }
*)
%}

%%

(* Typed and tagged rhs inside parentheses *
 *******************************************
 * Handles rhs constraints between parentheses.
 *
 * Parameters:
 *   X (rhs): The rhs to constrain
 *   allow_paren (+bool): whether the expression can begin with an opening parenthesis
 *
 * Returns an rhs
 *)
%public %inline ttpar(X, allow_paren): fun_in_par(tt, X, allow_paren) { $1 }


(* Add a type and tag constraint *
 *********************************
 * Adds a type and tag constraint to an rhs.
 *
 * Parameters:
 *   X (rhs): the rhs to constrain
 *
 * Returns an rhs
 *)
%public %inline tt(X):
  | X                      { $1 }
  //| X either(tag, of_type) { let (tag, ty) = $2 in rcons $1 tag ty }
  | X of_type { let (tag, ty) = $2 in rcons $1 tag ty }
