{
  open Parser_impl
  open Ast
  open Exceptions

  let rewind lexbuf n =
    (* TODO we should ensure that n is not too large *)
    let open Lexing in
    lexbuf.lex_curr_p   <- { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - n; } ;
    lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n

  let add_char c (s, l) = (s ^ (String.make 1 c), l)
  let close_string = function
    | "", l -> l
    | s, l  -> Plain s::l
  let add_expr e = function
    | "", l -> ("", Expr e::l)
    | s, l  -> ("", Expr e::Plain s::l)

  type lexer_state = Default | SQuote of Location.t | DQuote of Location.t
  let lexer_stack = ref [Default]

  let string_of_char_list cl = String.concat "" (List.map (String.make 1) cl)

  let string_of_acc acc = string_of_char_list (List.rev acc)

  let print_endline = ignore
  let print_string = ignore
  let print_char = ignore
}

let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let uid = '_'* ['A'-'Z'] alphanum*
let lid = '_'* ['a'-'z' '0'-'9'] alphanum*
let whitespace = [' ' '\t']+
let eol = ('\n' '\r' | '\r' '\n' | '\n' | '\r')
let noneol = [^ '\n' '\r']
let esc = ['\\' '$' '\'' '"']
let op_char = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' '#']
let preop_char = ['!' '#']
let postop_char_0 = '|'
let postop_char_1 = '&'
let postop_char_2 = '='
let postop_char_3 = ['+' '-']
let postop_char_4 = ['*' '/' '%']

rule token = parse
  | eof                    { print_endline"EOF";EOF }
  | _ { rewind lexbuf 1;
        (match !lexer_stack with
         | Default::_            -> default
         | SQuote lex_start_p::_ -> squote lex_start_p []
         | DQuote lex_start_p::_ -> dquote lex_start_p []
         | []                    -> failwith "Internal_failure") lexbuf }

and default = parse
  | "**" noneol* eol       { print_endline"COMMENT"; Location.incr_line lexbuf; token lexbuf }
  | "**" noneol* eof       { print_endline"EOF";EOF }
  | "also"                 { token lexbuf }
  | "component"            { print_endline "COMPONENT(`component`)"; COMPONENT }
  | "interface"            { print_endline"INTERFACE";INTERFACE }
  | "let"                  { print_endline"LET(`let`)"; LET }
  | "type"                 { print_endline"TYPE(`type`)"; TYPE }
  | "for"                  { print_endline "FOR(`for`)"; FOR }
  | "all"                  { print_endline "ALL(`all`)"; ALL }
  | "exists"               { print_endline "EXISTS(`exists`)"; EXISTS }
  | "if"                   { print_endline "IF(`if`)"; IF }
  | "then"                 { print_endline "THEN(`then`)"; THEN }
  | "else"                 { print_endline "ELSE(`else`)"; ELSE }
  | "{*"                   { comment lexbuf }
  | '('                    { print_endline "LPAREN(`(`)"; LPAREN }
  | ')'                    { print_endline "RPAREN(`)`)"; RPAREN }
  | "<:"                   { print_endline "SUBTYPEOF(`<:`)"; SUBTYPEOF }
  | ';'                    { print_endline "COLON(`;`)"; COLON }
  | ','                    { print_endline"COMMA";COMMA }
  | "::"                   { print_endline"PRAGMA";PRAGMA }
  | ':'                    { print_endline "OFTYPE(`:`)";OFTYPE }
  | '?'                    { print_endline"QMARK";QMARK }
  | "<-"                   { print_endline"SET";SET }
  | '<' '<'                { print_string "LITEXP(`"; litexpr [] lexbuf }
  | '<'                    { print_endline"LABRAC";LABRAC }
  | '>'                    { print_endline"RABRAC";RABRAC }
  | "[]"                   { print_endline"SQUARE";SQUARE }
  | '['                    { print_endline"LBRACK";LBRACK }
  | ']'                    { print_endline"RBRACK";RBRACK }
  | '{'                    { print_endline "LBRACE(`{`)"; LBRACE }
  | '}'                    { if List.length !lexer_stack > 1
                             then (lexer_stack := List.tl !lexer_stack; print_endline"EEXPR"; EEXPR)
                             else (print_endline "RBRACE(`}`)"; RBRACE) }
  | '='                    { print_endline "EQUAL(`=`)"; EQUAL }
  | "!=" as s              { print_endline "POSTOP2(`!=`)"; POSTOP2 s }
  | "..."                  { print_endline"ETC";ETC }
  | '.'                    { print_endline "DOT(`.`)"; DOT }
  | "->"                   { print_endline"ARROW";ARROW }
  | '@'                    { print_endline"AT";AT }
  | '^'                    { print_endline"HOOK";HOOK }
  | '+'                    { print_endline"PLUS";PLUS }
  | '-'                    { print_endline"MINUS";MINUS }
  | '*'                    { print_endline "STAR(`*`)"; STAR }
  | '!'                    { print_endline "BANG(`!`)"; BANG }
  | "!<<"                  { print_endline "BANG(`!`)"; rewind lexbuf 2; BANG }
  | preop_char op_char* as s { print_endline ("PREOP(`" ^ s ^ "`)"); PREOP s }
  | postop_char_0 op_char* as s { print_endline ("POSTOP0(`" ^ s ^ "`)"); POSTOP0 s }
  | "or"                   { print_endline "POSTOP0(`&`)"; POSTOP0 "|" }
  | postop_char_1 op_char* as s { print_endline ("POSTOP1(`" ^ s ^ "`)"); POSTOP1 s }
  | "and"                  { print_endline "POSTOP1(`&`)"; POSTOP1 "&" }
  | postop_char_2 op_char* as s { print_endline ("POSTOP2(`" ^ s ^ "`)"); POSTOP2 s }
  | postop_char_3 op_char+ as s { print_endline"POSTOP3"; POSTOP3 s }
  | postop_char_4 op_char* as s { print_endline"POSTOP4"; POSTOP4 s }
  | '`' ([^'`']+ as s) '`' { print_endline"WORD";WORD s }
  | uid as s               { print_endline ("UID(`" ^ s ^ "`)"); UID s }
  | lid as s               { print_endline ("LID(`" ^ s ^ "`)"); LID s }
  | '"'                    { print_endline"BSTRING"; lexer_stack := (DQuote (Location.curr lexbuf))::!lexer_stack; BSTRING }
  | '\''                   { print_endline"BSTRING"; lexer_stack := (SQuote (Location.curr lexbuf))::!lexer_stack; BSTRING }
  | eol                    { Location.incr_line lexbuf; token lexbuf }
  | whitespace             { token lexbuf }

and dquote lex_start_p acc = parse
  | '"' { print_endline("ESTRING "^(string_of_acc acc)); lexer_stack := List.tl !lexer_stack; ESTRING (string_of_acc acc) }
  | _   { rewind lexbuf 1; string dquote lex_start_p acc lexbuf }

and squote lex_start_p acc = parse
  | '\'' { print_endline("ESTRING "^(string_of_acc acc)); lexer_stack := List.tl !lexer_stack; ESTRING (string_of_acc acc) }
  | _    { rewind lexbuf 1; string squote lex_start_p acc lexbuf }

and string next lex_start_p acc = parse
  | '\\' (esc as e) { print_endline"ESC"; next lex_start_p (e::acc) lexbuf }
  | '\\'            { illegal_escape (Location.curr lexbuf) }
  | "${"            { print_endline("STRING "^(string_of_acc acc)); lexer_stack := Default::!lexer_stack; STRING (string_of_acc acc) }
  | eol             { quote_open lex_start_p }
  | eof             { quote_open lex_start_p }
  | _ as c          { next lex_start_p (c::acc) lexbuf }

and litexpr acc = parse
  | '>' '>' { print_endline "`)"; LITEXP (string_of_acc acc) }
  | eol     { litexpr_open (Location.curr lexbuf) }
  | eof     { litexpr_open (Location.curr lexbuf) }
  | _ as c  { print_char c; litexpr (c::acc) lexbuf }

and comment = parse
  | '*' '}' { token lexbuf }
  | eol     { Location.incr_line lexbuf; comment lexbuf }
  | _       { comment lexbuf }
