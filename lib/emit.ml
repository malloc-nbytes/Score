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

open Ast

module Emit = struct
  let g_ctx = Llvm.global_context ()
  let g_module = Llvm.create_module g_ctx "main"
  let g_builder = Llvm.builder g_ctx
  (* let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10 *)

  let create_attr llctx s = Llvm.create_enum_attr llctx s 0L

  let compile_expr_unary expr =
    failwith ""

  let compile_expr_binary expr =
    failwith ""

  let compile_expr_term expr =
    failwith ""

  let compile_expr = function
    | Ast.Term _ -> failwith ""
    | Ast.Binary _ -> failwith ""
    | Ast.Unary _ -> failwith ""

  let compile_stmt_while stmt =
    failwith ""

  let compile_stmt_mut stmt =
    failwith ""

  let compile_stmt_if stmt =
    failwith ""

  let compile_stmt_for stmt =
    failwith ""

  let compile_stmt_return stmt =
    failwith ""

  let compile_stmt_expr stmt =
    failwith ""

  let compile_stmt_block stmt =
    failwith ""

  let compile_stmt_proc stmt =
    failwith ""

  let compile_stmt_let stmt =
    failwith ""

  let compile_stmt = function
    | Ast.Let _ -> failwith ""
    | Ast.Proc _ -> failwith ""
    | Ast.Block _ -> failwith ""
    | Ast.Stmt_Expr _ -> failwith ""
    | Ast.Return _ -> failwith ""
    | Ast.For _ -> failwith ""
    | Ast.If _ -> failwith ""
    | Ast.Mut _ -> failwith ""
    | Ast.While _ -> failwith ""

  let compile_toplvl_stmt = function
    | Ast.Proc_Def _ -> failwith ""
    | Ast.Let _ -> failwith ""
    | Ast.Module _ -> failwith ""
    | Ast.Import _ -> failwith ""
    | Ast.Struct _ -> failwith ""

  let emit_ir program =
    let rec aux = function
      | [] -> ()
      | hd :: tl ->
         let _ = compile_toplvl_stmt hd in
         aux tl in
    aux program

end
