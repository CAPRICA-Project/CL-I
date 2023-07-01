open Exceptions
module I = Parser_impl.MenhirInterpreter
module Incremental = Parser_impl.Incremental

let fail (lexbuf:Lexing.lexbuf) = function
  | I.HandlingError env -> begin
      match I.top env with
      | Some (Element (state, _, _, _)) -> begin
          let msg = try Parser_errors.message (I.number state) with
            | Not_found -> "Unexpected token" in
          Location.print (Location.symbol_loc lexbuf.lex_start_p lexbuf.lex_curr_p);
          raise (Parsing_error (msg, Location.symbol_loc lexbuf.lex_start_p lexbuf.lex_curr_p))
        end
      | None -> assert false
    end
  | _ -> assert false

let parse file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf file;
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let checkpoint = Incremental.start lexbuf.lex_curr_p in
  let ast = I.loop_handle (fun x -> x) (fail lexbuf) supplier checkpoint in
  close_in ic;
  ast
