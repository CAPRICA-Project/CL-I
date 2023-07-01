(* This public partial grammar provides independent and generic parameterized rules used in the
 * other partial grammars.
 *)
%%

(* More list *
 *************
 * Handles separated lists of at least 2 elements
 *
 * Parameters:
 *   SEP: the separator symbol
 *   X ('a): the list element symbol
 *
 * Returns an 'a list
 *)
%public more_list(SEP, X): X SEP separated_nonempty_list(SEP, X) { $1::$3 }


(* Some *
 ********
 * Wraps symbols in the Some constructor.
 *
 * Parameter:
 *   X ('a): the symbol to consider
 *
 * Returns an 'a option
 *)
%public some(X): X { Some $1 }


(* Inside parentheses *
 **********************
 * Handles symbols between parentheses.
 *
 * Parameters:
 *   X ('a): the symbol to consider
 *
 * Returns an 'a
 *)
%public inpar(X): LPAREN X RPAREN { $2 }


(* Either *
 **********
 * Allows a token, another, or both in the specified order.
 *
 * Parameters:
 *   X ('a): the first token
 *   Y ('b): the other token
 *
 * Returns an 'a option * 'b option tuple
 *)
%public %inline either(X, Y):
  | X    { (Some $1, None) }
  | X? Y { ($1, Some $2) }

%public %inline any2(X, Y): X | Y { $1 }
%public %inline any3(X, Y, Z): X | Y | Z { $1 }
