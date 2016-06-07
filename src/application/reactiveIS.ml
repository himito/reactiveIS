open Types
open Lexer
open Interpreter

(** function that parses the program **)
let process_of_file =
  let filename = Sys.argv.(1) in
  Parser.main Lexer.lex (Lexing.from_channel (open_in filename))

let _ = interpreterIS process_of_file
