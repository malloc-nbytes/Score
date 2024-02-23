(* MIT License

   * Copyright (c) 2023 malloc-nbytes

   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:

   * The above copyright notice and this permission notice shall be included in all
   * copies or substantial portions of the Software.

   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   * SOFTWARE. *)

open Token
open Lexer
open Parser
open Ast
open Gen

let file_to_str filepath =
  let ch = open_in_bin filepath in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let write_to_file filepath content =
  let oc = open_out filepath in
  let _ = Printf.fprintf oc "%s" content in
  close_out oc

let run_qbe filepath =
  let cmd = Printf.sprintf "qbe -o out.s %s" filepath in
  (* TODO: Deal with exit failure *)
  let _ = Sys.command cmd in ()

let assemble () =
  let cmd = "cc -o out out.s" in
  let _ = Sys.command cmd in ()

let () =
  let infp = "./input.txt" in
  let data = file_to_str infp in
  let _ = Lexer.populate_keywords () in

  let tokens = Lexer.lex_file (String.to_seq data |> List.of_seq) 1 1 in
  (* let _ = Lexer.print_tokens tokens in *)

  let program = Parser.produce_ast tokens in
  (* Ast.ast_dump program; *)

  let code : string = Gen.generate_inter_lang program in
  let _ = print_endline code in

  let outfp = "./out.ssa" in
  let _ = write_to_file outfp code in
  let _ = run_qbe outfp in

  let _ = assemble () in ()
