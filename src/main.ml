open Token
open Lexer
open Parser
open Ast

let file_to_str filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

(* let repl_produce_ast () = *)
(*   let filepath = "./input.txt" in *)
(*   let data = file_to_str filepath in *)
(*   let _ = Lexer.populate_keywords () in *)
(*   let tokens = Lexer.lex_file (String.to_seq data |> List.of_seq) 1 1 in *)
(*   (\* let _ = Lexer.print_tokens tokens in *\) *)
(*   Parser.produce_ast tokens *)

let () =
  let filepath = "./input.txt" in
  let data = file_to_str filepath in
  let _ = Lexer.populate_keywords () in
  let tokens = Lexer.lex_file (String.to_seq data |> List.of_seq) 1 1 in
  (* let _ = Lexer.print_tokens tokens in *)
  let ast = Parser.produce_ast tokens in
  Ast.ast_dump ast
;;
