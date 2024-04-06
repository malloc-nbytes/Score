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

let import_deps : ((string, string list) Hashtbl.t) = Hashtbl.create 20
let compiled_files : (string list) ref = ref []

let get_ast (filepath : string) : Ast.program =
  let src_code = Utils.file_to_str filepath in
  let tokens = Lexer.lex_file filepath (String.to_seq src_code |> List.of_seq) 1 1 in
  Parser.produce_ast tokens

let rec make (filepath : string) : (string * Ast.program) list =
  if List.mem filepath !compiled_files then []
  else
    let ast = get_ast filepath in
    let imports = Module.gather_imports ast in

    compiled_files := [filepath] @ !compiled_files;
    Hashtbl.add import_deps filepath imports;

    let rec aux = function
      | [] -> []
      | hd :: tl -> make hd @ aux tl in

    (filepath, ast) :: aux imports

(* debugging *)
let dump_modules mods =
  List.iter (fun m ->
      let open Module in
      Printf.printf "modname: %s\n" m.modname;
      Printf.printf "depends:\n";

      List.iter (fun d ->
          Printf.printf "  %s\n" d.modname
        ) m.Module.depends;

      Printf.printf "ast:\n";
      Ast.dump_toplvl_stmts m.ast;
      print_endline "") mods

let () =
  ignore dump_modules;

  let filepath = "input.scr" in

  Lexer.populate_keywords ();
  print_endline "[ Compiling ]";

  let asts = make filepath in
  let ast_tbl : (string, Ast.program) Hashtbl.t = Hashtbl.create 5 in
  List.iter (fun t -> Hashtbl.add ast_tbl (fst t) (snd t)) asts;
  let modules : Module.t list = Module.produce_modules ast_tbl import_deps in
  ignore modules;

  print_endline "[ Done ]"

