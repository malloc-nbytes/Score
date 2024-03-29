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

open Utils
open Token
open Lexer
open Preprocessor
open Parser
open Ast
open Proc
open Ir

let input_filepath = ref ""
let output_ssa = ref ""
let output_asm = ref ""
let output_bin = ref ""
let ssas : string list ref = ref []
let asms : string list ref = ref []

let qbe fp =
  output_asm := fp^".s";
  asms := !asms @ [!output_asm];
  let ssa_files = (String.concat " " !ssas) in

  print_endline ssa_files;

  let cmd = Printf.sprintf "qbe -o %s %s" !output_asm fp in
  let exit_code = Sys.command cmd in
  if exit_code <> 0 then
    let _ = Printf.printf " ERR (exit: %d)\n" exit_code in
    exit 1

let cc () =
  output_bin := !input_filepath^".out";
  let asms = (String.concat " " !asms) in
  print_endline asms;
  let cmd = Printf.sprintf "cc -g -o %s %s" !output_bin asms in
  let exit_code = Sys.command cmd in
  if exit_code <> 0 then
    let _ = Printf.printf " ERR (exit: %d)\n" exit_code in
    exit 1

let rec compile input_filepath =
  let src_code = Utils.file_to_str input_filepath in

  let tokens = Lexer.lex_file input_filepath (String.to_seq src_code |> List.of_seq) 1 1 in

  let tokens = Preprocessor.insert_macros tokens in

  let tree = Parser.produce_ast tokens in

  let imports = Proc.populate_proc_tbl tree in
  List.iter (fun imp -> compile imp) imports;

  let ircode = Ir.generate_ir tree in

  output_ssa := input_filepath^".ssa";
  ssas := !output_ssa :: !ssas;
  Utils.write_to_file !output_ssa ircode

let () =

  let argv = Array.to_list Sys.argv in
  let argv = List.tl argv in

  input_filepath := List.hd argv;
  output_ssa := !input_filepath^".ssa";
  output_asm := !input_filepath^".s";

  Lexer.populate_keywords ();

  print_endline "[ Compiling ]";

  compile !input_filepath;

  List.iter qbe !ssas;

  cc ();

  print_endline "[ Done ]"



