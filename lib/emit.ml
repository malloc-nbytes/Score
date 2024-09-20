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
  type variable =
    { id : string
    ; tok : Token.t
    ; llvm_value : Llvm.llvalue
    }

  type context =
    { vscope : ((string, variable) Hashtbl.t) list
    ; fscope : ((Token.t, Llvm.llvalue) Hashtbl.t) list
    ; func : Llvm.llvalue option
    ; builder : Llvm.llbuilder
    }

  let ctx = Llvm.global_context ()
  let md = Llvm.create_module ctx "global_mod"
  let bl = Llvm.builder ctx

  (* --- TYPES --- *)

  let i32_t context = Llvm.i32_type ctx
  let void_t context = Llvm.i32_type ctx

  (* --- UTILITY --- *)

  let push_scope context : context =
    let vscope = Hashtbl.create 10 :: context.vscope in
    {context with vscope}

  let pop_scope context : context =
    match context.vscope, context.fscope with
    | [_], _ | _, [_] ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"cannot pop scope of size 1" None in
       exit 1
    | _, _ -> {context with vscope=List.tl context.vscope}

  let scope_add_variable id value context =
    match context.vscope with
    | [] ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"no scope to add variable to" None in
       exit 1
    | hd :: _ -> Hashtbl.add hd id value

  let rec scope_contains_variable id context =
    let rec aux = function
      | [] -> false
      | hd :: tl ->
        if Hashtbl.mem hd id then true
        else false in
    aux context.vscope

  let rec scope_get_variable id context =
    let rec aux = function
      | [] -> failwith @@ Printf.sprintf "%s: variable %s is not in scope" __FUNCTION__ id
      | hd :: tl ->
        (match Hashtbl.find_opt hd id with
        | Some v -> v
        | None -> aux tl) in
    aux context.vscope

  let rec assert_variables_not_in_vscope ids context =
    List.iter (fun id ->
      match scope_contains_variable id context with
      | true ->
        let var = scope_get_variable id context in
        let msg = Printf.sprintf "variable `%s` has already been declared" id in
        let _ = Err.err Err.Redeclaration __FILE__ __FUNCTION__ ~msg @@ Some var.tok in
        exit 1
      | false -> ()
    ) ids

  let create_attr ?(defval=0L) context s : Llvm.llattribute =
    Llvm.create_enum_attr ctx s defval

  let build_global_str value context : Llvm.llvalue =
    let s = Llvm.build_global_stringptr value "" bl in
    let zero = Llvm.const_int (Llvm.i32_type ctx) 0 in
    Llvm.build_in_bounds_gep (Llvm.i32_type ctx) s [|zero|] "" bl

  let emit_entry_alloca
        (func : Llvm.llvalue) (var_name : string)
        (ty : Llvm.lltype) (context : context)
      : Llvm.llvalue =
    let builder = Llvm.builder_at (ctx) (Llvm.instr_begin (Llvm.entry_block func)) in
    Llvm.build_alloca ty var_name builder

  let scr_ty_to_llvm_ty ty context =
    let open TokenType in
    match ty with
    | I32 -> Llvm.i32_type ctx
    | Void -> Llvm.void_type ctx
    | _ -> failwith @@ Printf.sprintf "%s unhandled type: %s" __FUNCTION__ (string_of_id_type ty)

  (* --- COMPILATION --- *)

  let rec compile_expr_unary expr context : Llvm.llvalue =
    failwith ""

  let compile_expr_binary expr context : Llvm.llvalue =
    failwith ""

  let compile_expr_term expr context : Llvm.llvalue =
    match expr with
    | Ast.IntLit i -> Llvm.const_int (i32_t context) (int_of_string i.lexeme)
    | _ -> failwith "todo: compile_expr_term"

  let compile_expr expr context : Llvm.llvalue =
    match expr with
    | Ast.Term e -> compile_expr_term e context
    | Ast.Binary _ -> failwith "Binary"
    | Ast.Unary _ -> failwith "Unary"

  let rec compile_stmt_while stmt context : context =
    failwith "todo: compile_stmt_while"

  and compile_stmt_mut stmt context : context =
    failwith "todo: compile_stmt_mut"

  and compile_stmt_if stmt context : context =
    failwith "todo: compile_stmt_if"

  and compile_stmt_for stmt context : context =
    failwith "todo: compile_stmt_for"

  and compile_stmt_return expr context : context =
    let value = compile_expr expr context in
    let _ = Llvm.build_ret value context.builder in
    context

  and compile_stmt_expr stmt context : context =
    failwith "todo: compile_stmt_expr"

  and compile_stmt_block stmt context : context =
    match stmt with
    | [] -> context
    | hd :: tl ->
      let context = compile_stmt hd context in
      compile_stmt_block tl context

  and compile_stmt_proc Ast.{id; params; rettype; block; export} context : context =
    let ret_ty = scr_ty_to_llvm_ty rettype context in
    let param_tys = List.map (fun ty -> scr_ty_to_llvm_ty ty context) @@ List.map snd params in

    let proc_ty = Llvm.function_type ret_ty (Array.of_list param_tys) in
    let proc_def = Llvm.define_function id.lexeme proc_ty md in

    let context = push_scope context in

    let rec aux (params : (Token.t * TokenType.id_type) list) context =
      match params with
      | [] -> context
      | hd :: tl ->
         let id, tok = (fst hd).lexeme, (fst hd) in
         let _ = assert_variables_not_in_vscope [id] context in
         let alloca = emit_entry_alloca proc_def ((fst hd).Token.lexeme) (scr_ty_to_llvm_ty (snd hd) context) context in
         let var = {id; tok; llvm_value=alloca} in
         let _ = scope_add_variable id var context in
         aux tl context in

    let builder = Llvm.builder_at ctx (Llvm.instr_begin (Llvm.entry_block proc_def)) in
    let context = aux params context in
    let context = {context with func = Some proc_def;
                                builder = builder} in
    let context = compile_stmt_block block context in
    context

  and compile_stmt_let Ast.{id; ty; expr; _} context : context =
    let _ = match context.func with
      | None -> failwith "no function to add variable to"
      | Some _ -> () in

    let _ = assert_variables_not_in_vscope [id.lexeme] context in
    let ty = scr_ty_to_llvm_ty ty context in

    let local_var = Llvm.build_alloca ty id.lexeme context.builder in
    let value = compile_expr expr context in
    let _ = Llvm.build_store value local_var context.builder in
    let var = {id=id.lexeme; tok=id; llvm_value=local_var} in
    let _ = scope_add_variable id.lexeme var context in
    context

  and compile_stmt_module stmt context : context =
    failwith "todo: compile_stmt_module"

  and compile_stmt_import stmt context : context =
    failwith "todo: compile_stmt_import"

  and compile_stmt_struct stmt context : context =
    failwith "todo: compile_stmt_struct"

  and compile_stmt stmt context : context =
    match stmt with
    | Ast.Let s -> compile_stmt_let s context
    | Ast.Proc _ -> failwith ""
    | Ast.Block _ -> failwith ""
    | Ast.Stmt_Expr _ -> failwith ""
    | Ast.Return s -> compile_stmt_return s context
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
    let vscope = [Hashtbl.create 10] and fscope = [Hashtbl.create 10] in
    let context = {vscope; fscope; func=None; builder=bl} in

    let rec aux toplvl_stmts context =
      match toplvl_stmts with
      | [] -> context
      | hd :: tl ->
         let context = compile_toplvl_stmt hd context in
         aux tl context in

    let _ = aux program context in
    Llvm.dump_module md
end
