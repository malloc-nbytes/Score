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

let compile_exe obj_file output_executable =
  let cmd = Printf.sprintf "gcc -o %s %s -fPIE" output_executable obj_file in
  let result = Unix.system cmd in
  match result with
  | WEXITED 0 -> ()
  | _ -> failwith "failed to compile the executable"

let compile_module md outfp =
  let level = Llvm_target.CodeGenOptLevel.Default in
  let code_model = Llvm_target.CodeModel.Default in
  let reloc_mode = Llvm_target.RelocMode.PIC in

  let target_triple = Llvm_target.Target.default_triple () in
  let target = Llvm_target.Target.by_triple target_triple in
  let target_machine = Llvm_target.TargetMachine.create ~triple:target_triple ~level ~code_model ~reloc_mode target in

  let result = Llvm_target.TargetMachine.emit_to_file md Llvm_target.CodeGenFileType.ObjectFile outfp target_machine in
  result

let () =
  let filepath = "./src/input.scr" in
  let src_code = Utils.file_to_str filepath
                 |> String.to_seq
                 |> List.of_seq in

  Lexer.populate_keywords ();
  (* print_endline "[ Compiling ]"; *)

  let tokens = Lexer.lex_file filepath src_code 1 1 in
  let ast = Parser.produce_ast tokens in
  let context = Emit.emit_ir filepath ast in

  let _ = Llvm_executionengine.initialize () in

  let main_ir = Llvm.string_of_llmodule context.md in
  let other_modules_ir = let rec aux = function
                           | [] -> []
                           | hd :: tl -> [Llvm.string_of_llmodule hd.Emit.md] @ aux tl in
                         aux context.children_contexts in

  (* let _ = Printf.printf "MAIN MODULE:\n%s\n" main_ir in *)
  (* let _ = List.iteri (fun i s -> Printf.printf "MODULE %d: \n%s\n" i s) other_modules_ir in *)

  let obj_filename = "LLVM_object_file" in
  let _ = compile_module context.md obj_filename in
  compile_exe obj_filename "a.out"
