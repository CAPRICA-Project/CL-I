open Cl1_compiler

let () =
  ignore (Compiler.Base.compile (Parser.parse Sys.argv.(1)));
  print_endline "---Done---"
