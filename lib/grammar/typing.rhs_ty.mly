(* This partial grammar provides rules for rhs types. *)

%token ARROW "->"

%%


%public %inline rhs_type:
  | UID ioption(rhs_type_params)
      { Rtype { ty_name = $1; ty_params = $2; ty_sub = [] } }
  | UID ioption(rhs_type_params) cardinality+
      { Rtlist { ty = Rtype { ty_name = $1; ty_params = $2; ty_sub = [] }; cardinality = $3 } }

%public dot_rhs_ty:
  | dot_rhs DOT rhs_type    { Rrtdot { dot_obj = $1; dot_mbr = $3 } }
  | dot_rhs_ty DOT rhs_type { Rtrtdot { dot_obj = $1; dot_mbr = $3 } }
  | rhs_type                { $1 }

%inline cardinality:
  | PLUS                                    { From (Rsym "1") }
  | STAR                                    { From (Rsym "0") }
  | SQUARE                                  { From_to (Smth, Smth) }
  | LBRACK rhs(False) RBRACK                { From_to ($2, $2) }
  | LBRACK rhs(False) ETC rhs(False) RBRACK { From_to ($2, $4) }
  | LBRACK rhs(False) ETC RBRACK            { From $2 }
  | LBRACK ETC rhs(False) RBRACK            { To $3 }


(* rhs type parameters *
 ***********************
 * Handles rhs type parameters.
 *
 * Returns an rhs_ty option arg
 *)
%public rhs_type_params:
  | type_params(lax(typespec_or_word)) { $1 }
  | WORD                               { [Arg (Some (Word $1))] }


(* Type specification or word *
 ******************************
 * Handles template parameters.
 *
 * Returns an rhs_ty
 *)
%public typespec_or_word:
  | typespec { $1 }
  | WORD     { Word $1 }


(* Type hint *
 *************
 * Handles type hints (value : Type%tags).
 *
 * Returns an rhs_ty
 *)
%public of_type: OFTYPE typespec { $2 }


(* Type specification *
 **********************
 * Handles type specifications.
 *
 * Returns an rhs_ty
 *)
%public typespec:
  | tagged_type                   { $1 }
  | arrow_list                    { unpack2 (func []) $1 }
  //| typename_implicit+ arrow_list { unpack2 (func $1) $2 }


(* Tagged type *
 ***************
 * Handles types with tags.
 *
 * Returns an rhs_ty
 *)
//%public tagged_type: typename tag? { tagged $1 $2 }
%public tagged_type: typename { tagged $1 None }


(* Explicit part of a typename *
 *******************************
 * Handles explicit typename components.
 *
 * Returns an rhs_ty
 *)
%public typename:
  | dot_rhs_ty           { $1 }
  | tuple(typespec)      { Rttuple $1 }
  | LPAREN RPAREN        { Unit }
//  | TYPE                 { Built_in $1 }


(* Left part of an arrow list *
 ******************************
 * Handles left parts of arrow lists (the arrow lists without the return types).
 *
 * Returns an rhs_ty option arg list
 *)
left_arrow_list:
  | arg(lax(tagged_type))                       { [$1] }
  | left_arrow_list ARROW arg(lax(tagged_type)) { $1 @ [$3] }


(* Arrow list *
 **************
 * Handles arrow lists.
 *
 * Returns an rhs_ty option arg list * rhs_ty option
 *)
%public arrow_list: left_arrow_list ARROW lax(tagged_type) { ($1, $3) }
