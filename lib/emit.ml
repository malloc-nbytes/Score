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
open Types

module Emit = struct
  type function_ =
    { rettype : TokenType.id_type
    ; variadic : bool
    }

  type symbol_type =
    | Function of function_
    | Variable of TokenType.id_type

  type symbol =
    { tok : Token.t
    ; ty : symbol_type
    ; value : Llvm.llvalue option
    ; _module : Token.t
    }

  type context =
    { func : Llvm.llvalue option
    ; builder : Llvm.llbuilder
    ; symtbl : ((string, symbol) Hashtbl.t list)
    ; last_sym : symbol option
    ; _module : Token.t option
    ; ctx : Llvm.llcontext
    ; md : Llvm.llmodule
    ; imports : (string, context) Hashtbl.t
    }

  (* --- UTILITY --- *)

  let get_specific_import id context =
    match Hashtbl.find_opt context.imports id with
    | Some ctx -> ctx
    | None -> failwith @@ Printf.sprintf "%s: could not find import: `%s`" __FUNCTION__ id

  let rec strip_pointer_type ty =
    let open TokenType in
    match ty with
    | Pointer p ->
       (match p with
        | Pointer k -> strip_pointer_type k
        | _ -> p)
    | _ -> failwith "tried to strip pointer type from non-pointer type"

  let add_target_triple triple llm =
    let lltarget = Llvm_target.Target.by_triple triple in
    let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
    let lldly = Llvm_target.TargetMachine.data_layout llmachine in
    Llvm.set_target_triple (Llvm_target.TargetMachine.triple llmachine) llm;
    Llvm.set_data_layout (Llvm_target.DataLayout.as_string lldly) llm

  let push_scope context : context =
    {context with symtbl = Hashtbl.create 10 :: context.symtbl}

  let pop_scope context : context =
    if List.length context.symtbl = 0 then
      let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"cannot pop scope of size 0" None in
      exit 1
    else {context with symtbl = List.tl context.symtbl}

  let add_symbol id tok ty value _module context : unit =
    let sym = {tok; ty; value; _module} in
    match context.symtbl with
    | [] -> failwith "cannot add symbol because the symtbl is empty"
    | hd :: _ -> Hashtbl.add hd id sym

  let get_symbol id context : symbol =
    let rec aux = function
      | [] -> failwith @@ Printf.sprintf "%s: could not find symbol: `%s`" __FUNCTION__ id
      | hd :: tl ->
         (match Hashtbl.find_opt hd id with
          | Some sym -> sym
          | _ -> aux tl) in
    aux context.symtbl

  let get_symbol_wmodule (id : string) (_module : string) context : symbol =
    let rec aux = function
      | [] -> failwith @@ Printf.sprintf "%s: could not find symbol: `%s`" __FUNCTION__ id
      | hd :: tl ->
         (match Hashtbl.find_opt hd id with
          | Some (sym : symbol) ->
              (match sym._module.lexeme = _module with
               | true -> sym
               | false -> aux tl)
          | _ -> aux tl) in
    aux context.symtbl

  let assert_symbol_noexist id context : unit =
    let rec aux = function
      | [] -> ()
      | hd :: tl ->
         if Hashtbl.mem hd id then
           failwith @@ Printf.sprintf "%s: symbol already exists: `%s`" __FUNCTION__ id
         else aux tl in
    aux context.symtbl

  let create_attr ?(defval=0L) context s : Llvm.llattribute =
    Llvm.create_enum_attr context.ctx s defval

  let build_global_str value context : Llvm.llvalue =
    Llvm.build_global_stringptr value "" context.builder

  let emit_entry_block_alloca
        (func : Llvm.llvalue) (var_name : string)
        (ty : Llvm.lltype) (context : context)
      : Llvm.llvalue =
    let builder = Llvm.builder_at (context.ctx) (Llvm.instr_begin (Llvm.entry_block func)) in
    Llvm.build_alloca ty var_name builder

  let scr_ty_to_llvm_ty ty context =
    let open TokenType in
    match ty with
    | I32 -> Llvm.i32_type context.ctx
    | I8 -> Llvm.i8_type context.ctx
    | Usize -> Llvm.i64_type context.ctx
    | Void -> Llvm.void_type context.ctx
    | Str -> Llvm.pointer_type context.ctx
    | Pointer _ -> Llvm.pointer_type context.ctx
    | _ -> failwith @@ Printf.sprintf "%s unhandled type: %s" __FUNCTION__ (string_of_id_type ty)

  let llvm_ty_to_scr_ty ty =
    let open TokenType in
    match Llvm.classify_type ty with
    | Llvm.TypeKind.Integer -> I32
    | Llvm.TypeKind.Void -> Void
    | Llvm.TypeKind.Pointer -> Str
    | _ -> failwith @@ Printf.sprintf "%s unhandled type: %s" __FUNCTION__ (Llvm.string_of_lltype ty)

  let assert_types_compat ty1 ty2 =
    if not (Types.typecheck ty1 ty2) then
      failwith @@ Printf.sprintf "%s: type mismatch: `%s` and `%s`" __FUNCTION__ (TokenType.string_of_id_type ty1) (TokenType.string_of_id_type ty2)
    else ()

  (* --- COMPILATION --- *)

  let rec compile_expr_unary expr context : Llvm.llvalue =
    failwith "compile_expr_unary: todo"

  and compile_expr_binary Ast.{lhs; op; rhs} context : Llvm.llvalue * context =
    let open Token in
    let open TokenType in

    let lhs, context = compile_expr lhs context in
    let rhs, context = compile_expr rhs context in
    let _ = assert_types_compat
            (llvm_ty_to_scr_ty (Llvm.type_of lhs))
            (llvm_ty_to_scr_ty (Llvm.type_of rhs)) in
    match op.ttype with
    | Plus -> Llvm.build_add lhs rhs "addtmp" context.builder, context
    | Minus -> Llvm.build_sub lhs rhs "subtmp" context.builder, context
    | Asterisk -> Llvm.build_mul lhs rhs "multmp" context.builder, context
    | ForwardSlash -> Llvm.build_sdiv lhs rhs "divtmp" context.builder, context
    | LessThan -> Llvm.build_icmp Llvm.Icmp.Slt lhs rhs "cmptmp" context.builder, context
    | GreaterThan -> Llvm.build_icmp Llvm.Icmp.Sgt lhs rhs "cmptmp" context.builder, context
    | DoubleEquals -> Llvm.build_icmp Llvm.Icmp.Eq lhs rhs "cmptmp" context.builder, context
    | _ -> failwith @@ Printf.sprintf "%s: unhandled binary operator: %s" __FUNCTION__ op.lexeme

  and compile_expr_intlit i context =
    let open Token in
    Llvm.const_int (Llvm.i32_type context.ctx) (int_of_string i.lexeme), context

  and compile_expr_strlit s context =
    let open Token in
    build_global_str s.lexeme context, context

  and compile_expr_ident i context =
    let open Token in
    let sym = get_symbol i.lexeme context in
    let context = {context with last_sym=Some sym} in
    let ty = scr_ty_to_llvm_ty (match sym.ty with
      | Function f -> f.rettype
      | Variable ty -> ty) context in
    match sym.value with
    | Some value -> Llvm.build_load ty value sym.tok.lexeme context.builder, context
    | None -> failwith @@ Printf.sprintf "%s: value is None" __FUNCTION__

  and compile_expr_proc_call Ast.{lhs; args} context =
    (* let proc = compile_expr lhs context in *)
    let (args : Llvm.llvalue array) = List.map (fun expr -> let value, _ = compile_expr expr context in value) args |> Array.of_list in
    let callee = match Llvm.lookup_function lhs.lexeme context.md with
      | None ->
         let _ = Err.err Err.Undeclared __FILE__ __FUNCTION__ ~msg:"function does not exist" @@ Some lhs in
         exit 1
      | Some proc -> proc in
    let stored_proc = (get_symbol lhs.lexeme context) in
    let stored_proc_ty = scr_ty_to_llvm_ty (match stored_proc.ty with
                                            | Function f -> f.rettype
                                            | Variable ty -> ty) context in
    let ty = Llvm.function_type stored_proc_ty (Array.of_list
      (List.map (fun x -> Llvm.type_of x) (Array.to_list (Llvm.params callee))
    )) in

    let is_variadic = match stored_proc.ty with
      | Function f -> f.variadic
      | _ -> false in

    if is_variadic then
      let va_arg_ty = Llvm.var_arg_function_type stored_proc_ty (Array.of_list
        (List.map (fun x -> Llvm.type_of x) (Array.to_list (Llvm.params callee))
      )) in
      Llvm.build_call va_arg_ty callee args "" context.builder, context
    else
      Llvm.build_call ty callee args "" context.builder, context

  and compile_expr_index Ast.{accessor; idx} context =
    let open Token in

    let accessor_value, context = compile_expr accessor context in

    let stripped_ty = match context.last_sym with
      | Some s -> (match s.ty with
                   | Function ty -> ty.rettype
                   | Variable ty -> ty)
      | None -> failwith "cannot strip ty" in

    let stripped_ty = strip_pointer_type stripped_ty in
    let stripped_ty = scr_ty_to_llvm_ty stripped_ty context in

    let index_value, context = compile_expr idx context in

    (* Get the pointer to the indexed element *)
    let pointer = Llvm.build_in_bounds_gep stripped_ty accessor_value [|index_value|] "indexptr" context.builder in
    Llvm.build_load (Llvm.i32_type context.ctx) pointer "loadtmp" context.builder, context

  and compile_expr_cast (Ast.{_type; rhs} : Ast.expr_cast) context : Llvm.llvalue * context =
    let open TokenType in
    let rhs_value, context = compile_expr rhs context in
    let rhs_type = llvm_ty_to_scr_ty (Llvm.type_of rhs_value) in
    let _ = Types.typecheck_for_casting _type rhs_type in

    let casted = match _type with
      | I32 -> Llvm.build_intcast rhs_value (Llvm.i32_type context.ctx) "casttmp" context.builder
      | I8 -> Llvm.build_intcast rhs_value (Llvm.i8_type context.ctx) "casttmp" context.builder
      | Usize -> Llvm.build_intcast rhs_value (Llvm.i64_type context.ctx) "casttmp" context.builder
      | Str -> Llvm.build_bitcast rhs_value (Llvm.pointer_type context.ctx) "casttmp" context.builder
      | Pointer _ -> Llvm.build_bitcast rhs_value (scr_ty_to_llvm_ty _type context) "casttmp" context.builder
      | _ -> failwith "unhandled cast" in

    casted, context

  and compile_expr_namespace (Ast.{left; right} : Ast.expr_namespace) context =
    let open Token in
    let id = left.lexeme in
    (* let import_context = get_specific_import id context in *)
    (* let updated_context = {context with symtbl = import_context.symtbl} in *)
    match right with
    | Ast.Ident right_id -> failwith "compile_expr_namespace: Ast.Ident: todo"
       (* let sym = get_symbol right_id.lexeme updated_context in *)
       (* let ty = scr_ty_to_llvm_ty (match sym.ty with *)
       (*                             | Function f -> f.rettype *)
       (*                             | Variable ty -> ty) updated_context in *)
       (* (match sym.value with *)
       (*  | Some value -> Llvm.build_load ty value right_id.lexeme updated_context.builder, context *)
       (*  | None -> failwith @@ Printf.sprintf "%s: value is None" __FUNCTION__) *)
    | Ast.Proc_Call pc ->
       (* let res, context = compile_expr_proc_call pc {import_context with builder=context.builder} in *)
       failwith ""
       (* let res, context' = compile_expr_proc_call pc {import_context with builder=context.builder} in *)
       (* res, {context with builder=context'.builder} *)
    | _ ->
       failwith "unhandled right expression type in namespace"

  and compile_expr_term expr context : Llvm.llvalue * context =
    match expr with
    | Ast.IntLit i -> compile_expr_intlit i context
    | Ast.StrLit s -> compile_expr_strlit s context
    | Ast.Ident i -> compile_expr_ident i context
    | Ast.Proc_Call pc -> compile_expr_proc_call pc context
    | Ast.Index i -> compile_expr_index i context
    | Ast.Cast c -> compile_expr_cast c context
    | Ast.Namespace n -> compile_expr_namespace n context
    | _ -> failwith "todo: compile_expr_term"

  and compile_expr expr context : Llvm.llvalue * context =
    match expr with
    | Ast.Term e -> compile_expr_term e context
    | Ast.Binary e -> compile_expr_binary e context
    | Ast.Unary _ -> failwith "Unary"

  and compile_stmt_mut Ast.{left; op; right} context : context =
    (* Compile the left-hand side expression to get the variable *)
    (* let left_value = compile_expr left context in *)

    (* Ensure the left expression is a variable *)
    (match left with
     | Ast.Term (Ast.Ident id) ->
        let sym = get_symbol id.lexeme context in
        (* Ensure it's a variable and has a value *)
        (match sym.ty with
         | Variable varty ->
            (match sym.value with
             | Some llvm_value ->
                (* Compile the right-hand side expression *)
                let right_value, context = compile_expr right context in

                let llvm_ty = llvm_ty_to_scr_ty (Llvm.type_of right_value) in
                let _ = assert_types_compat varty llvm_ty in

                (* Build a store instruction to update the variable's value *)
                let _ = Llvm.build_store right_value llvm_value context.builder in
                context
             | None ->
                failwith @@ Printf.sprintf "%s: variable `%s` has no associated LLVM value" __FUNCTION__ id.lexeme)
         | _ ->
            failwith @@ Printf.sprintf "%s: symbol `%s` is not a variable" __FUNCTION__ id.lexeme)
     | Ast.Term (Ast.Index {accessor; idx}) ->
        let accessor_value, context = compile_expr accessor context in

        let stripped_ty = match context.last_sym with
          | Some s -> (match s.ty with
                       | Function ty -> ty.rettype
                       | Variable ty -> ty)
          | None -> failwith "cannot strip ty" in
        let stripped_ty = strip_pointer_type stripped_ty in
        let stripped_ty = scr_ty_to_llvm_ty stripped_ty context in

        let index_value, context = compile_expr idx context in

        (* Get the pointer to the indexed element *)
        let pointer = Llvm.build_in_bounds_gep stripped_ty accessor_value [|index_value|] "indexptr" context.builder in

        (* Compile the right-hand side expression to get the new value *)
        let right_value, context = compile_expr right context in

        (* Store the new value at the indexed pointer *)
        let _ = Llvm.build_store right_value pointer context.builder in
        context
     | _ ->
        failwith "Left-hand side of mutation must be an identifier")

  (* NOTE: It is up to the caller of this function
   * to push () and pop () the scope. This is because it
   * makes it easier to keep track of proc params! *)
   and compile_stmt_block stmt context : context =
    let rec aux lst context =
      match lst with
      | [] -> context
      | hd :: tl ->
         let context = compile_stmt hd context in
         aux tl context in
    aux stmt context

  and compile_stmt_if Ast.{expr; block; _else} context : context =
    let ctx = Llvm.global_context () in
    let func = Option.get context.func in

    (* Create blocks for the if statement *)
    let then_block = Llvm.append_block ctx "then" func in
    (* let else_block = Llvm.append_block ctx "else" func in *)
    let else_block = match _else with
      | Some _ -> Some (Llvm.append_block ctx "else" func)
      | None -> None in

    (* Create the merge block *)
    let merge_block = Llvm.append_block ctx "ifcont" func in

    (* Compile the condition *)
    let cond_value, context = compile_expr expr context in

    (* Create the conditional branch based on the condition *)
    let _ = match else_block with
      | Some else_block ->
        Llvm.build_cond_br cond_value then_block else_block context.builder
      | None ->
        Llvm.build_cond_br cond_value then_block merge_block context.builder in

    (* Position the builder at the then block *)
    let builder = Llvm.builder_at ctx (Llvm.instr_begin then_block) in
    let context = {context with builder = builder} in

    (* Compile the then block *)
    let context = compile_stmt_block block context in

    (* Create a branch to the merge block from the then block *)
    let _ = Llvm.build_br merge_block context.builder in

    (* Position the builder at the else block *)
    let builder = match else_block with
      | Some else_block -> Llvm.builder_at ctx (Llvm.instr_begin else_block)
      | None -> Llvm.builder_at ctx (Llvm.instr_begin merge_block) in
    let context = {context with builder = builder} in

    (* Compile the else block *)
    match _else with
    | Some _else ->
      let context = compile_stmt_block _else context in
      let _ = Llvm.build_br merge_block context.builder in
      {context with builder = Llvm.builder_at ctx (Llvm.instr_begin merge_block)}
    | None ->
      {context with builder = Llvm.builder_at ctx (Llvm.instr_begin merge_block)}

  and compile_stmt_for Ast.{start; _while; _end; block} context : context =
    let context = push_scope context in

    (* Compile the start statement to initialize the loop variable *)
    let context = compile_stmt start context in

    (* Create the condition block and the loop body block *)
    let loop_cond_block = Llvm.append_block context.ctx "loop_cond" (Option.get context.func) in
    let loop_body_block = Llvm.append_block context.ctx "loop_body" (Option.get context.func) in
    let loop_end_block = Llvm.append_block context.ctx "loop_end" (Option.get context.func) in

    (* Create a branch to the condition block from the entry block *)
    let _ = Llvm.build_br loop_cond_block context.builder in

    (* Position the builder at the condition block *)
    let builder = Llvm.builder_at context.ctx (Llvm.instr_begin loop_cond_block) in
    (* context.builder <- builder; *)
    let context = {context with builder = builder} in

    (* Compile the while condition *)
    let cond_value, context = compile_expr _while context in

    (* Create the conditional branch based on the condition *)
    let _ = Llvm.build_cond_br cond_value loop_body_block loop_end_block builder in

    (* Position the builder at the body block *)
    let builder = Llvm.builder_at context.ctx (Llvm.instr_begin loop_body_block) in
    let context = {context with builder = builder} in

    (* Compile the loop body *)
    let context = compile_stmt_block block context in

    (* Update statement (e.g., increment) *)
    let context = compile_stmt _end context in

    let context = pop_scope context in

    (* Unconditionally branch back to the condition block *)
    let _ = Llvm.build_br loop_cond_block context.builder in

    (* Position the builder at the end block *)
    let context = {context with builder = Llvm.builder_at context.ctx (Llvm.instr_begin loop_end_block)} in

    context

  and compile_stmt_while (Ast.{expr; block} : Ast.stmt_while) context : context =
    let context = push_scope context in

    (* Create the condition block and the loop body block *)
    let loop_cond_block = Llvm.append_block context.ctx "loop_cond" (Option.get context.func) in
    let loop_body_block = Llvm.append_block context.ctx "loop_body" (Option.get context.func) in
    let loop_end_block = Llvm.append_block context.ctx "loop_end" (Option.get context.func) in

    (* Create a branch to the condition block from the entry block *)
    let _ = Llvm.build_br loop_cond_block context.builder in

    (* Position the builder at the condition block *)
    let builder = Llvm.builder_at context.ctx (Llvm.instr_begin loop_cond_block) in
    let context = {context with builder = builder} in

    (* Compile the while condition *)
    let cond_value, context = compile_expr expr context in

    (* Create the conditional branch based on the condition *)
    let _ = Llvm.build_cond_br cond_value loop_body_block loop_end_block builder in

    (* Position the builder at the body block *)
    let builder = Llvm.builder_at context.ctx (Llvm.instr_begin loop_body_block) in
    let context = {context with builder = builder} in

    (* Compile the loop body *)
    let context = compile_stmt_block block context in

    (* Unconditionally branch back to the condition block *)
    let _ = Llvm.build_br loop_cond_block context.builder in

    (* Position the builder at the end block *)
    let context = {context with builder = Llvm.builder_at context.ctx (Llvm.instr_begin loop_end_block)} in
    let context = pop_scope context in
    context

  and compile_stmt_return expr context : context =
    let value, context = compile_expr expr context in
    let _ = Llvm.build_ret value context.builder in
    context

  and compile_stmt_expr stmt context : context =
    let _ = compile_expr stmt context in
    context

  and compile_stmt_proc Ast.{id; params; rettype; block; variadic} context : context =
    (* Create the return type and parameter types *)
    let ret_ty = scr_ty_to_llvm_ty rettype context in
    let param_tys = List.map (fun ty -> scr_ty_to_llvm_ty ty context) @@ List.map snd params in

    (* Define and create the function *)
    let proc_ty = Llvm.function_type ret_ty (Array.of_list param_tys) in
    let proc_def = Llvm.define_function id.lexeme proc_ty context.md in

    (* Add function to the scope *)
    let _ = add_symbol id.lexeme id (Function {rettype; variadic}) (Some proc_def) (Option.get context._module) context in

    let context = push_scope context in

    (* Alloc all parameters on the stack and register
     * all of them into the scope. *)
    let rec aux params acc context =
      let open Token in
      match params with
      | [] -> context, acc
      | hd :: tl ->
         let id, tok = (fst hd).lexeme, (fst hd) in
         let _ = assert_symbol_noexist id context in
         let alloca = emit_entry_block_alloca proc_def ((fst hd).Token.lexeme) (scr_ty_to_llvm_ty (snd hd) context) context in
         let _ = add_symbol id tok (Variable (snd hd)) (Some alloca) (Option.get context._module) context in
         aux tl (acc @ [alloca]) context in

    (* Position the builder *)
    let builder = Llvm.builder_at context.ctx (Llvm.instr_begin (Llvm.entry_block proc_def)) in

    let context, values = aux params [] context in

    List.iter (fun (param, value) ->
        let _ = Llvm.build_store value param builder in
    ()) (List.combine values (Array.to_list (Llvm.params proc_def)));

    (* Register the current procedure and the builder position. *)
    let context = {context with func = Some proc_def;
                                builder = builder} in

    (* Iterate over the statements in the procedure body *)
    let context = compile_stmt_block block context in
    pop_scope context

  and compile_stmt_let Ast.{id; ty; expr; _} context : context =
    let _ = match context.func with
      | None -> failwith "no function to add variable to"
      | Some _ -> () in

    let _ = assert_symbol_noexist id.lexeme context in
    let llvm_ty = scr_ty_to_llvm_ty ty context in

    let local_var = Llvm.build_alloca llvm_ty id.lexeme context.builder in
    let value, context = compile_expr expr context in
    let _ = Llvm.build_store value local_var context.builder in
    let _ = add_symbol id.lexeme id (Variable ty) (Some local_var) (Option.get context._module) context in

    context

  and compile_stmt_module (Ast.{id} : Ast.stmt_module) context : context =
    {context with _module = Some id}

  and compile_stmt_import (Ast.{filepath} : Ast.stmt_import) context : context =
    let filepath = filepath.lexeme in
    let src_code = Utils.file_to_str filepath |> String.to_seq |> List.of_seq in
    let tokens = Lexer.lex_file filepath src_code 1 1 in
    let ast = Parser.produce_ast tokens in
    let import_context_module = Ast.get_module_name ast in
    let import_context = emit_ir filepath ast in
    (* let _ = Hashtbl.add context.imports import_context_module import_context in *)
    import_context

  and compile_stmt_struct stmt context : context =
    failwith "todo: compile_stmt_struct"

  and compile_stmt_def (Ast.{id; params; rettype; variadic; _} : Ast.stmt_def) context : context =
    failwith "compile_stmt_def: todo"

  and compile_stmt_extern (Ast.{id; params; rettype; variadic; _} : Ast.stmt_extern) context : context =
    (* Create the return type and parameter types *)
    let ret_ty = scr_ty_to_llvm_ty rettype context in
    let param_tys = List.map (fun ty -> scr_ty_to_llvm_ty ty context) @@ List.map snd params in

    (* Define and create the function *)
    let proc_ty = if variadic then
                    Llvm.var_arg_function_type ret_ty (Array.of_list param_tys)
                  else
                    Llvm.function_type ret_ty (Array.of_list param_tys) in

    let proc_decl = Llvm.declare_function id.lexeme proc_ty context.md in

    let _ = Llvm.add_function_attr proc_decl (create_attr context "nounwind") (Llvm.AttrIndex.Function) in
    (* let _ = Llvm.add_function_attr proc_decl (create_attr context "nocapture") (Llvm.AttrIndex.Param 0) in *)
    let _ = add_symbol id.lexeme id (Function {rettype; variadic}) (Some proc_decl) (Option.get context._module) context in
    context

  and compile_stmt stmt context : context =
    match stmt with
    | Ast.Let s -> compile_stmt_let s context
    | Ast.Proc _ -> failwith "todo: compile_stmt: proc"
    | Ast.Block _ -> failwith "todo: compile_stmt: block"
    | Ast.Stmt_Expr e -> compile_stmt_expr e context
    | Ast.Return s -> compile_stmt_return s context
    | Ast.For f -> compile_stmt_for f context
    | Ast.If _if -> compile_stmt_if _if context
    | Ast.Mut m -> compile_stmt_mut m context
    | Ast.While w -> compile_stmt_while w context

  and compile_toplvl_stmt toplvl_stmt context : context =
    match toplvl_stmt with
    | Ast.Proc_Def stmt -> compile_stmt_proc stmt context
    | Ast.Let stmt -> compile_stmt_let stmt context
    | Ast.Module stmt -> compile_stmt_module stmt context
    | Ast.Import stmt -> compile_stmt_import stmt context
    | Ast.Struct stmt -> compile_stmt_struct stmt context
    | Ast.Def stmt -> compile_stmt_def stmt context
    | Ast.Extern stmt -> compile_stmt_extern stmt context

  and emit_ir (src_filename : string) program : context =
    let ctx = Llvm.global_context () in
    let md = Llvm.create_module ctx src_filename in
    let bl = Llvm.builder ctx in
    let symtbl = [Hashtbl.create 10] in
    let context = {func=None;
                   builder=bl;
                   symtbl;
                   last_sym=None;
                   _module=None;
                   ctx;
                   md;
                   imports=Hashtbl.create 3} in

    let rec aux toplvl_stmts context =
      match toplvl_stmts with
      | [] -> context
      | hd :: tl ->
         let context = compile_toplvl_stmt hd context in
         aux tl context in



    let context = aux program context in
    (* let _ = print_endline @@ Llvm.string_of_llmodule md in *)
    Llvm_analysis.assert_valid_module context.md;
    context
end
