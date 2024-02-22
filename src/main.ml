open Token
open Lexer
open Parser
open Ast
open Gen

let file_to_str filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let () =
  let filepath = "./input.txt" in
  let data = file_to_str filepath in
  let _ = Lexer.populate_keywords () in

  let tokens = Lexer.lex_file (String.to_seq data |> List.of_seq) 1 1 in
  (* let _ = Lexer.print_tokens tokens in *)

  let program = Parser.produce_ast tokens in
  (* Ast.ast_dump program; *)

  let intermediate_code : string = Gen.generate_inter_lang program in
  let _ = print_endline intermediate_code in

  let oc = open_out "out.ssa" in
  let _ = Printf.fprintf oc "%s" intermediate_code in
  close_out oc
;;
