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
open Err
open Token

module Emit = struct
  type context =
    { ctx : Llvm.llcontext
    ; md : Llvm.llmodule
    ; bl : Llvm.llbuilder
    ; vscope : ((string, Llvm.llvalue) Hashtbl.t) list
    ; fscope : ((string, Llvm.llvalue) Hashtbl.t) list
    }

  (* --- TYPES --- *)

  let i32_t context = Llvm.i32_type context.ctx
  let void_t context = Llvm.i32_type context.ctx

  (* --- UTILITY --- *)

  let push_scope context : context = {context with vscope = context.vscope @ [Hashtbl.create 10];
                                                   fscope = context.fscope @ [Hashtbl.create 10]}
  let pop_scope context : context =
    match context.vscope, context.fscope with
    | [_], _ | _, [_] ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"cannot pop scope of size 1" None in
       exit 1
    | _ -> {context with vscope = List.rev @@ List.tl @@ List.rev context.vscope;
                         fscope = List.rev @@ List.tl @@ List.rev context.fscope}

  let create_attr ?(defval=0L) context s : Llvm.llattribute =
    Llvm.create_enum_attr context.ctx s defval

  let build_global_str value context : Llvm.llvalue =
    let s = Llvm.build_global_stringptr value "" context.bl in
    let zero = Llvm.const_int (Llvm.i32_type context.ctx) 0 in
    Llvm.build_in_bounds_gep (Llvm.i32_type context.ctx) s [|zero|] "" context.bl

  let create_entry_alloca
        (func : Llvm.llvalue) (var_name : string)
        (ty : Llvm.lltype) (context : context)
      : Llvm.llvalue =
    let builder = Llvm.builder_at (context.ctx) (Llvm.instr_begin (Llvm.entry_block func)) in
    Llvm.build_alloca ty var_name builder

  let scr_ty_to_llvm_ty rettype context =
    let open TokenType in
    match rettype with
    | I32 -> Llvm.i32_type context.ctx
    | Void -> Llvm.void_type context.ctx
    | _ -> failwith @@ Printf.sprintf "%s unhandled type: %s" __FUNCTION__ (string_of_id_type rettype)

  (* --- COMPILATION --- *)

  let compile_expr_unary expr context =
    failwith ""

  let compile_expr_binary expr context =
    failwith ""

  let compile_expr_term expr context =
    failwith ""

  let compile_expr context = function
    | Ast.Term _ -> failwith ""
    | Ast.Binary _ -> failwith ""
    | Ast.Unary _ -> failwith ""

  let compile_stmt_while stmt context : context =
    failwith "todo: compile_stmt_while"

  let compile_stmt_mut stmt context : context =
    failwith "todo: compile_stmt_mut"

  let compile_stmt_if stmt context : context =
    failwith "todo: compile_stmt_if"

  let compile_stmt_for stmt context : context =
    failwith "todo: compile_stmt_for"

  let compile_stmt_return stmt context : context =
    failwith "todo: compile_stmt_return"

  let compile_stmt_expr stmt context : context =
    failwith "todo: compile_stmt_expr"

  let compile_stmt_block stmt context : context =
    failwith "todo: compile_stmt_block"

  let compile_stmt_proc Ast.{id; params; rettype; block; export} context : context =
    let ret_ty = scr_ty_to_llvm_ty rettype context in
    let param_tys = List.map (fun ty -> scr_ty_to_llvm_ty ty context) @@ List.map snd params in
    let param_names = List.map fst params in

    let proc_ty = Llvm.function_type ret_ty (Array.of_list param_tys) in
    let proc_def = Llvm.define_function id.lexeme proc_ty context.md in

    context

  let compile_stmt_let stmt context : context =
    failwith "todo: compile_stmt_let"

  let compile_stmt_module stmt context : context =
    failwith "todo: compile_stmt_module"

  let compile_stmt_import stmt context : context =
    failwith "todo: compile_stmt_import"

  let compile_stmt_struct stmt context : context =
    failwith "todo: compile_stmt_struct"

  let compile_stmt stmt context : context =
    match stmt with
    | Ast.Let _ -> failwith ""
    | Ast.Proc _ -> failwith ""
    | Ast.Block _ -> failwith ""
    | Ast.Stmt_Expr _ -> failwith ""
    | Ast.Return _ -> failwith ""
    | Ast.For _ -> failwith ""
    | Ast.If _ -> failwith ""
    | Ast.Mut _ -> failwith ""
    | Ast.While _ -> failwith ""

  let compile_toplvl_stmt toplvl_stmt context : context =
    match toplvl_stmt with
    | Ast.Proc_Def stmt -> compile_stmt_proc stmt context
    | Ast.Let stmt -> compile_stmt_let stmt context
    | Ast.Module stmt -> compile_stmt_module stmt context
    | Ast.Import stmt -> compile_stmt_import stmt context
    | Ast.Struct stmt -> compile_stmt_struct stmt context

  let emit_ir program =
    let ctx = Llvm.global_context () in
    let md = Llvm.create_module ctx "global_mod" in
    let bl = Llvm.builder ctx in
    let vscope = [Hashtbl.create 10] and fscope = [Hashtbl.create 10] in
    let context = {ctx; md; bl; vscope; fscope} in

    let rec aux toplvl_stmts context =
      match toplvl_stmts with
      | [] -> context
      | hd :: tl ->
         let context = compile_toplvl_stmt hd context in
         aux tl context in

    let context = aux program context in
    Llvm.dump_module context.md
end
