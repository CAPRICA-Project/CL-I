%{
  open Ast
  open Arg

  (*let ($>) a b = let temp = a in (b; temp)*)
  let unpack2 f t = let (e, e') = t in f e e'

  (*let last_adj = ref None*)

  let func fun_implicit args fun_ret =
    let fun_args = if List.length args = 0 then [Arg (Some Unit)] else args in
    Fun { fun_implicit; fun_args; fun_ret }
(*
  let adj adj_implicit adj_arg = Adj { adj_implicit; adj_arg }
*)

  let tagged tagged_ty = function
    | Some tagged_tag -> Tagged { tagged_ty; tagged_tag }
    | None -> tagged_ty
  (*let type_ ty_name ty_params ty_sub ty_kind = Rtype { ty_name; ty_params; ty_sub; ty_kind }
  let init ty_name ty_params tag init_body =
    Rinit { init_ty = tagged (type_ ty_name ty_params None None) tag; init_body }*)

(*
  let last_sym = ref 0
*)
  let ref_sym = ref (Symset.empty)
%}

%token EOF "<EOF>"

%token EEXPR "}"

/* Identifiers */
%token <string> UID "Uppercase_ID" LID "lowercase_ID" /* LOOSE */

/* Operators */
%token EQUAL "="
%token DOT "."
%token SUBTYPEOF "<:" OFTYPE ":"
%token PLUS "+" MINUS "-" STAR "*" BANG "!"
%token <string> PREOP "#" POSTOP0 "or" POSTOP1 "and" POSTOP2 "=>" POSTOP3 "+$" POSTOP4 "%%" LITEXP "<<literal expression>>"
%token IF "if" THEN "then" ELSE "else"

/* Pairs */
%token LPAREN "(" RPAREN ")"
%token LABRAC "<" RABRAC ">"
%token LBRACK "[" RBRACK "]" SQUARE "[]"
%token LBRACE "{ " RBRACE " }"
%token BSTRING "\""
%token <string> ESTRING "string\""

/* Typing */
//%token VULNTO MKEACH BECOME PROPAGATES
//%token DEFTO

/* Misc */
%token COLON ";" COMMA ","
%token <string> WORD "`word`"

/* Values */
%token <string> STRING "string${"
%nonassoc _ifte
%right ELSE
%left POSTOP0
%left POSTOP1
%left POSTOP2 EQUAL LABRAC RABRAC
%left PLUS MINUS POSTOP3 LITEXP
%left STAR POSTOP4
%nonassoc _postfix
%nonassoc BANG PREOP


/* Entrypoint */
%start start
%type <Ast.t * Ast.Symset.t> start

%%

start: structures_or_defs EOF { let ast = $1 in (ast, !ref_sym) }


(* Structure or lhs *
 ********************
 * Handles structures and rhs.
 *
 * Returns a construct_body
 *)
structure_or_def:
  | structure | def { $1 }
  | set         { [Init $1] }
  | rhs(True)   { [Rexpr $1] }

%public structures_or_defs: structure_or_def* { List.flatten $1 }
