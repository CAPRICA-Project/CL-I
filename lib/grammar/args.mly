(* This partial grammar provides rules for args. *)

%token ETC "..."

%%

(* Argument *
 ************
 * Handles arguments, i.e. symbols or ellipses.
 *
 * Parameter:
 *   X ('a): the symbol for the argument
 *
 * Returns an 'a arg
 *)
%public arg(X):
  | X   { Arg $1 }
  | ETC { Etc }


(* Lax *
 *******
 * Handles lax symbols, i.e. maybe unknown symbols.
 *
 * Parameter:
 *   X ('a): the symbol allowed to be lax
 *
 * Returns an 'a arg
 *)
%public lax(X):
  | X     { Some $1 }
  | QMARK { None }

(* Argument list *
 *****************
 * Handles argument lists. Two consecutive `...' are disallowed, a single `...' without other
 * arguments is also disallowed (for technical grammar reasons).
 *
 * Parameter:
 *   X ('a): the symbol for the argument
 *
 * Returns an 'a arg list
 *)
arg_list(X):
  | ETC COMMA arg_list_was_etc(X) { Etc::$3 }
  | X COMMA arg_list_continue(X)  { Arg $1::$3 }
  | X                             { [Arg $1] }

(*** This part prevents two consecutive `...' ***)
arg_list_was_etc(X):
  | X COMMA arg_list_continue(X) { Arg $1::$3 }
  | X                            { [Arg $1] }

(*** This is the safe continuation of the recursion ***)
arg_list_continue(X):
  | ETC COMMA arg_list_was_etc(X) { Etc::$3 }
  | X COMMA arg_list_continue(X)  { Arg $1::$3 }
  | ETC                           { [Etc] }
  | X                             { [Arg $1] }


(* Parameter list *
 ******************
 * Handles parameters inside parentheses.
 *
 * Parameter:
 *   X ('a): the symbol for each parameter
 *
 * Returns an 'a arg list
 *)
%public %inline parameter_list(X):
  | LPAREN RPAREN      { [] }
  | inpar(arg_list(X)) { $1 }
