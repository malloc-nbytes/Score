open Token
open Lexer

let file_to_str filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let filepath = "./input.txt"
let () =
  let data = file_to_str filepath in
  let tokens = Lexer.lex_file data in
  Lexer.print_tokens tokens
;;
