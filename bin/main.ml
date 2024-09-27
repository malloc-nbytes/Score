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

open Lib
open Emit
open Ast

let object_file_postfix = ".__LLVM_COMPILED_OBJECT_FILE"

let compile_exe obj_files output_executable =
  let cmd = Printf.sprintf "gcc -o %s %s -fPIE" output_executable obj_files in
  let result = Unix.system cmd in
  match result with
  | WEXITED 0 -> ()
  | _ -> failwith "failed to compile the executable"

let rec compile_modules lst acc =
  let compile_module md outfp =
    let level = Llvm_target.CodeGenOptLevel.Default in
    let code_model = Llvm_target.CodeModel.Default in
    let reloc_mode = Llvm_target.RelocMode.PIC in
    let target_triple = Llvm_target.Target.default_triple () in
    let target = Llvm_target.Target.by_triple target_triple in
    let target_machine = Llvm_target.TargetMachine.create ~triple:target_triple ~level ~code_model ~reloc_mode target in
    Llvm_target.TargetMachine.emit_to_file md Llvm_target.CodeGenFileType.ObjectFile outfp target_machine in
  match lst with
  | [] -> acc
  | hd :: tl ->
     let obj_filename = fst hd in
     let _ = compile_module (snd hd) (obj_filename^object_file_postfix) in
     compile_modules tl (acc @ [obj_filename])

let () =
  let filepath = "./src/input.scr" in
  let src_code = Utils.file_to_str filepath |> String.to_seq |> List.of_seq in

  let _ = Llvm_executionengine.initialize () in
  Lexer.populate_keywords ();

  let tokens = Lexer.lex_file filepath src_code 1 1 in
  let ast = Parser.produce_ast tokens in
  let context = Emit.emit_ir filepath ast in

  let modules = [Ast.get_module_name ast, context.md] @ Hashtbl.fold (fun k v a -> [k, v.Emit.md] @ a) context.imports [] in

  let obj_filenames =
    let rec aux = function
      | [] -> ""
      | hd :: tl -> hd ^ object_file_postfix ^ " " ^ aux tl in
    aux (compile_modules modules []) in

  let output_executable_filename = "LLVM_Compiled_Binary.out" in
  compile_exe obj_filenames output_executable_filename
