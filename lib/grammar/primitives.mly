(* This public partial grammar provides independent but specific rules used in the other partial
 * grammars.
 *)

%token QMARK "?"

%%


(* Expression converter *
 ************************
 * Converts an rhs value into an expr.
 *
 * Parameter:
 *   X (rhs): the rhs value to convert
 *
 * Returns an expr
 *)
%public to_expr(X): X { [$1] }
