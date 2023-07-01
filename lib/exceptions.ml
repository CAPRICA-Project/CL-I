exception Malformed_ast of int
exception Sanity_error of int
type err = Not_found | Redefined | Not_initializable | Mismatching
exception Value_error of err * string
exception Type_error of err * string
exception Kind_error of err * string
exception Not_implemented
exception Syntax_error of int * Location.t
type internal_failure = Not_processed | Not_allowed
exception Internal_failure of internal_failure * string
exception Parsing_error of string * Location.t

let illegal_escape loc = raise (Syntax_error (1, loc))
let quote_open loc = raise (Syntax_error (2, loc))
let litexpr_open loc = raise (Syntax_error (3, loc))
