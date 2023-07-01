(* This partial grammar provides rules for rhs values. *)

%{
(*
  let rscope obj scope_body =
    let rev = List.rev obj in
    List.fold_left (fun body scope_obj -> Rscope { scope_obj; scope_body = [body] })
                   (Rscope { scope_obj = List.hd rev; scope_body }) (List.tl rev)
*)
(*
  let sym_def s = Sym_def (!++last_sym, s)
  let sym_def s = s
*)
%}

%%

%public rhs_value:
  | tuple(rhs(False)) { Rtuple $1 }
  | dot_rhs           { $1 }

rhs_list:
  |                           { [] }
  | rhs(False)                { [$1] }
  | rhs(False) COLON rhs_list { $1::$3 }

%inline rhs_atom:
  //| id  { Rsym $1 }
  | LID { Rsym $1 }
  | str { $1 }
  | SQUARE { failwith "NOT IMPLEMENTED" }
  //| LBRACK rhs_list RBRACK { failwith "NOT IMPLEMENTED" }

%inline _rhs_atom:
  | LID { Rsym $1 }
  | str { $1 }
  | SQUARE { Rlist [] }
  | LBRACK rhs_list RBRACK { Rlist $2 }

%public dot_rhs:
  | dot_rhs DOT rhs_atom          { Rrdot { dot_obj = $1; dot_mbr = $3 } }
  | dot_rhs_ty DOT rhs_atom       { Rtrdot { dot_obj = $1; dot_mbr = $3 } }
  | _rhs_atom                     { $1 }
  | call(dot_rhs, rhs(False))     { $1 }
  | tcall(dot_rhs_ty, rhs(False)) { $1 }
  | dot_rhs LBRACE RBRACE         { $1 }
  | dot_rhs LBRACE set+ RBRACE    { Rinit { init_obj = $1; init_body = $3 } }
  | dot_rhs_ty LBRACE RBRACE      { tcall $1 None }
  | dot_rhs_ty LBRACE set+ RBRACE { Rinit { init_obj = tcall $1 None; init_body = $3 } }

preop:
  | PREOP { $1 }
  | BANG  { "!" }
  | PLUS  { "+" }
  | MINUS { "-" }

%inline postop:
  | POSTOP0 { Rsym $1 }
  | POSTOP1 { Rsym $1 }
  | POSTOP2 { Rsym $1 }
  | EQUAL   { Rsym "=" }
  | LABRAC  { Rsym "<" }
  | RABRAC  { Rsym ">" }
  | POSTOP3 { Rsym $1 }
  | PLUS    { Rsym "+" }
  | MINUS   { Rsym "-" }
  | LITEXP  { Rlitexpr $1 }
  | POSTOP4 { Rsym $1 }
  | STAR    { Rsym "*" }

%inline bang_postop:
  | BANG LITEXP       { Rlitexpr $2 }

%public rhs(has_colon):
  | rhs_value ift(has_colon, COLON)                    { $1 }
  | preop rhs(has_colon)                    { Call { call_obj = Rsym $1; call_args = [Arg (Some $2)] } } %prec PREOP
  | rhs(False) postop ift(has_colon, COLON) { Call { call_obj = $2; call_args = [Arg (Some $1)] } } %prec _postfix
  | rhs(False) postop rhs(has_colon)        { Call { call_obj = $2; call_args = [Arg (Some $1); Arg (Some $3)] } }
  | rhs(False) bang_postop ift(has_colon, COLON) {
      Call { call_obj = Rsym "!"; call_args = [Arg (Some (Call { call_obj = $2; call_args = [Arg (Some $1)] }))] } } %prec _postfix
  | rhs(False) bang_postop rhs(has_colon) {
      Call { call_obj = Rsym "!"; call_args = [Arg (Some (Call { call_obj = $2; call_args = [Arg (Some $1); Arg (Some $3)] }))] } }
  | LPAREN rhs(False) RPAREN ift(has_colon, COLON) { $2 }
  | z3_property ift(has_colon, COLON) { $1 }
  | IF rhs_value THEN rhs(has_colon) { Call { call_obj = Rsym "=>"; call_args = [Arg (Some $2); Arg (Some $4)] } } %prec _ifte
  | IF rhs_value THEN rhs(False) ELSE rhs(has_colon) { Call { call_obj = Rsym "=>"; call_args = [Arg (Some $2); Arg (Some $4); Arg (Some $6)] } } %prec _ifte

str: BSTRING _str ESTRING { let sl = match String.length $3 with
                                     | 0 -> $2
                                     | _ -> $2 @ [Rstring $3] in
                            match sl with
                            | []                -> Rstring ""
                            | [Rstring _ as rs] -> rs
                            | _                 -> Rstrcat sl }

_str:
  | STRING _str  { match String.length $1 with
                   | 0 -> $2
                   | _ -> Rstring $1::$2 }
  | rhs(False) EEXPR _str { $1::$3 }
  |              { [] }

(* Call *
 ********
 * Handles function calls.
 *
 * Parameters:
 *  X (rhs): The symbol before the parentheses
 *  Y (rhs): The symbols inside the parentheses
 *
 * Returns an rhs
 *)
call(X, Y): X parameter_list(lax(Y)) { call $1 $2 }
tcall(X, Y): X some(parameter_list(lax(Y))) { tcall $1 $2 }
