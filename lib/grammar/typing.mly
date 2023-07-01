(* This partial grammar provides parameterized typing rules. *)
%%

(* Type parameters *
 *******************
 * Handles type parameters between angle brackets.
 *
 * Parameters:
 *   X ('a): The symbol for the parameters
 *
 * Returns an 'a list
 *)
%public type_params(X): LABRAC separated_list(COMMA, arg(X)) RABRAC { $2 }


(* Tuple *
 *********
 * Handles tuples.
 *
 * Parameter:
 *   X ('a): the symbol for the tuple elements
 *
 * Returns an 'a
 *)
%public tuple(X):
  | inpar(more_list(COMMA, X)) { $1 }


(* Function inside parentheses *
 *******************************
 * Recurses on a function, putting each time parentheses around the result.
 *
 * Parameters:
 *   f ('a -> 'a): The function to apply
 *   X ('a): The symbol to apply the function on
 *   recurse: Whether to add parentheses and recurse or not
 *
 * Returns an 'a
 *)
%public %inline fun_in_par(f, X, recurse): ifte(recurse, _fun_in_par(f, X), f(X)) { $1 }

(*** This is the recursive part of the above rule ***)
_fun_in_par(f, X):
  | f(X)                         { $1 }
  | f(inpar(_fun_in_par(f, X))) { $1 }


(* Implicit part of a typename *
 *******************************
 * Handles implicit typename components.
 *
 * Returns an rhs
 *)
//%public typename_implicit: IMPLICIT ttpar(rhs_id, True) { $2 }
