(* This partial grammar provides rules for lhs types. *)
%%

(* Structure lhs type *
 **********************
 * Handles structure lhs types.
 *
 * Returns an lhs_ty
 *)
//%public lhs_struct_type: UID lhs_type_params? subtype_of_x? { Ltype { ty_name = $1; ty_params = $2; ty_sub = $3 } }


(* lhs type parameters *
 ***********************
 * Handles lhs type parameters.
 *
 * Returns a string list
 *)
%public lhs_type_params:
  | type_params(uid_or_word) { $1 }
  | WORD                     { [Arg (Tcons (Word $1))] }


(* UID or word *
 ***************
 * Handles template parameters.
 *
 * Returns an lhs_ty
 *)
%public uid_or_word:
  | UID  { Tuid $1 }
  | WORD { Tcons (Word $1) }


(* lhs type *
 ************
 * Handles non-structure lhs types.
 *
 * Returns an lhs_ty
 *)
%public lhs_type:
  | UID lhs_type_params?      { Ltype { ty_name = $1; ty_params = $2; ty_sub = [] } }
  | tuple(arg(lax(lhs_type))) { Lttuple $1 }
