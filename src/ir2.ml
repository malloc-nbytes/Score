module Ir2 = struct
  open Printf
  open Ast
  open Token
  open Err
  open Scope
  open Utils
  open Emit

  class label_maker =
    let init_reg = "__SCORE_REG" in

    object (self)
      val mutable reg = init_reg
      val mutable regc = 0

      method new_reg (global : bool) : string =
        let ret = (if global then "$" else "%") ^ reg ^ string_of_int regc in
        regc <- regc+1;
        ret
    end

  let lm = new label_maker

  (* --- Helpers --- *)

  let nums_compatable (type1 : TokenType.id_type) (type2 : TokenType.id_type) : bool =
    match type1, type2 with
    | a, b when a = b -> true
    (* I32 *)
    | TokenType.I32, TokenType.I32 -> true
    | TokenType.I32, TokenType.Number -> true
    | TokenType.Number, TokenType.I32 -> true
    (* Usize *)
    | TokenType.Usize, TokenType.Usize -> true
    | TokenType.Usize, TokenType.Number -> true
    | TokenType.Number, TokenType.Usize -> true
    (* Number *)
    | TokenType.Number, TokenType.Number -> true
    | _ -> false

  (* --- Evaluations --- *)

  let evaluate_binop = function
    | TokenType.Plus -> "add"
    | TokenType.Minus -> "sub"
    | TokenType.Asterisk -> "mul"
    | TokenType.ForwardSlash -> "div"
    | TokenType.DoubleEquals -> "ceqw"
    | TokenType.LessThan -> "csltw"
    | TokenType.GreaterThan -> "csgew"
    | TokenType.NotEqual -> "cnew"
    | TokenType.Percent -> "rem"
    | TokenType.DoubleAmpersand -> "and"
    | TokenType.DoublePipe -> "or"
    | TokenType.PlusEquals -> "add"
    | TokenType.MinusEquals -> "sub"
    | TokenType.AsteriskEquals -> "mul"
    | TokenType.PercentEquals -> "rem"
    | _ -> failwith "evaluate_binop: unimplemented binop"

  let rec evaluate_expr (expr : Ast.expr) : string * TokenType.id_type =
    match expr with
    | Ast.Binary bin ->
       let lhs, lhs_type = evaluate_expr bin.lhs in
       let rhs, rhs_type = evaluate_expr bin.rhs in
       let instr = evaluate_binop bin.op.ttype in
       let reg = lm#new_reg false in

       if nums_compatable lhs_type rhs_type then
         (* NOTE: can use either lhs_type or rhs_type *)
         let _ = Emit.binop reg lhs_type lhs rhs instr in
         reg, lhs_type
       else failwith @@ sprintf "%s: type mismatch: %s :: %s"
                          __FUNCTION__
                          (TokenType.id_type_to_string lhs_type)
                          (TokenType.id_type_to_string rhs_type)

    | Ast.Array_retrieval ar -> assert false

    | Ast.Term Ast.Ident ident ->
       Scope.assert_token_in_scope ident;
       let reg = lm#new_reg false in
       let stored_var = Scope.get_token_from_scope ident.lexeme in
       let stored_type = stored_var.type_ in
       if stored_var.stack_allocd then
         Emit.load reg stored_type ("%" ^ ident.lexeme)
       else
         Emit.copy reg stored_type ("%" ^ ident.lexeme);
       reg, stored_type

    | Ast.Term Ast.Intlit intlit ->
       intlit.lexeme, TokenType.Number

    | Ast.Dereference deref ->
      (match deref with
      | Ast.Term (Ast.Ident ident) ->
         Scope.assert_token_in_scope ident;
         let reg = lm#new_reg false in
         let stored_var = Scope.get_token_from_scope ident.lexeme in
         let stored_type = stored_var.type_ in
         Emit.load reg (stored_type) ("%" ^ ident.lexeme);
         let reg2 = lm#new_reg false in
         let inner_type = Utils.unwrap_ptr stored_type in
         Emit.load reg2 inner_type reg;
         reg2, inner_type
      | _ -> failwith "evaluate_expr: Ast.Dereference: unreachable")

    | Ast.Reference expr ->
       (match expr with
       | Ast.Term (Ast.Ident ident) ->
          Scope.assert_token_in_scope ident;
          let reg = lm#new_reg false in
          let stored_var = Scope.get_token_from_scope ident.lexeme in
          let stored_type = stored_var.type_ in
          Emit.copy reg (TokenType.Pointer stored_type) ("%" ^ ident.lexeme);
          reg, TokenType.Pointer stored_type
       | _ -> failwith "evaluate_expr: Ast.Reference: unreachable")

    | Ast.Cast (cast_type, expr) ->
       let expr, expr_type = evaluate_expr expr in
       if cast_type = expr_type then expr, cast_type
       else
         let reg = lm#new_reg false in
         (match cast_type, expr_type with
          | TokenType.I32, TokenType.Usize -> Emit.extsh reg cast_type expr; reg, cast_type
          | TokenType.Usize, TokenType.I32 -> Emit.extsw reg cast_type expr; reg, cast_type
          | _ -> failwith @@ sprintf "%s: cast error" __FUNCTION__)

    | Ast.Term Ast.Char chara -> assert false

    | Ast.Term Ast.Strlit strlit ->
       let reg = lm#new_reg true in
       Emit.string_in_data_section reg strlit.lexeme;
       reg, TokenType.Str

    | Ast.Term (Ast.IntCompoundLit (exprs, len)) -> assert false

    | Ast.Proc_call pc ->
       (* TODO: verify proc params match *)
       let args = List.fold_left (fun acc e ->
                      let arg, arg_type = evaluate_expr e in
                      let arg_type = Utils.scr_to_qbe_type arg_type in
                      acc ^ arg_type ^ " " ^ arg ^ ", "
                    ) "" pc.args in
       (match pc.id.lexeme with
        | "printf" ->
           let reg = lm#new_reg false in
           Emit.proc_call_wassign reg "printf" args TokenType.I32;
           reg, TokenType.I32
        | "exit" -> assert false
        | _ -> (* user-defined proc *)
           Scope.assert_proc_in_tbl pc.id.lexeme;
           let proc_rettype = Scope.get_proc_rettype_from_tbl pc.id.lexeme in
           (* TODO: assert proc params match *)
           let reg = lm#new_reg false in
           Emit.proc_call_wassign reg pc.id.lexeme args proc_rettype;
           reg, TokenType.I32)

  let evaluate_let_stmt (stmt : Ast.let_stmt) : unit =
    let id = stmt.id
    and id_lexeme = stmt.id.lexeme
    and stmt_type = stmt.type_ in

    Scope.assert_token_not_in_scope id;

    let expr, expr_type = evaluate_expr stmt.expr in
    let bytes = Utils.scr_type_to_bytes stmt_type in

    if nums_compatable stmt_type expr_type then
      let _ = Scope.add_id_to_scope id_lexeme id stmt_type true in
      let _ = Emit.stack_alloc4 id_lexeme bytes in
      Emit.store expr ("%" ^ id_lexeme) stmt_type
    else failwith @@ sprintf "%s: type mismatch: %s %s" __FUNCTION__
                       (TokenType.id_type_to_string stmt_type)
                       (TokenType.id_type_to_string expr_type)

  let rec evaluate_block_stmt (bs : Ast.block_stmt) : unit =
    Scope.push ();
    List.iter evaluate_stmt bs.stmts;
    Scope.pop ()

  and evaluate_proc_def_stmt (pd : Ast.proc_def_stmt) : unit =
    Scope.state.cur_proc_id <- pd.id.lexeme, pd.rettype;
    Scope.add_proc_to_tbl pd;

    Scope.push ();
    (* Make sure params are not in scope.
     * Add the params to the scope. *)
    List.iter (fun param ->
        let id = (fst param)
        and param_type = (snd param)
        and id_lexeme = (fst param).Token.lexeme in
        Scope.assert_token_not_in_scope id;
        match param_type with
        | TokenType.I32 | TokenType.Usize -> Scope.add_id_to_scope id_lexeme id param_type false
        | TokenType.Pointer _ -> Scope.add_id_to_scope id_lexeme id param_type true
        | _ -> failwith "evaluate_proc_def_stmt: unimplemented param type"
      ) pd.params;

    Emit.proc_def true pd.id.lexeme pd.params pd.rettype;
    evaluate_block_stmt pd.block;
    Scope.pop ();
    Emit.rbrace ()

  and evaluate_ret_stmt (stmt : Ast.ret_stmt) : unit =
    let expr, expr_type = evaluate_expr stmt.expr in
    ignore expr_type; (* TODO: make sure this matches cur_proc rettype *)
    Emit.ret expr

  and evaluate_stmt_expr (stmt : Ast.stmt_expr) : unit =
    let expr, type_ = evaluate_expr stmt in
    ignore expr;
    ignore type_

  and evaluate_mut_stmt (stmt : Ast.mut_stmt) : unit =
    match stmt with
    | Ast.Mut_var mutvar ->
       Scope.assert_token_in_scope mutvar.id;
       let expr, expr_type = evaluate_expr mutvar.expr
       and stored_var = Scope.get_token_from_scope mutvar.id.lexeme in
       let mut_type = stored_var.type_
       and mut_id = mutvar.id in
       if mut_type = expr_type then Emit.store expr ("%" ^ mut_id.lexeme) mut_type
       else failwith @@ sprintf "%s: type mismatch: %s <> %s" __FUNCTION__
                          (TokenType.id_type_to_string mut_type)
                          (TokenType.id_type_to_string expr_type)
    | Ast.Mut_arr mutarr -> failwith "evaluate_mut_stmt: mutarr unimplemented"

  and evaluate_stmt = function
    | Ast.Proc_def stmt -> assert false
    | Ast.Block stmt -> assert false
    | Ast.Let stmt -> evaluate_let_stmt stmt
    | Ast.Mut stmt -> evaluate_mut_stmt stmt
    | Ast.If stmt -> assert false
    | Ast.While stmt -> assert false
    | Ast.Stmt_expr stmt -> evaluate_stmt_expr stmt
    | Ast.Ret stmt -> evaluate_ret_stmt stmt
    | Ast.Break stmt -> assert false
    | Ast.For stmt -> assert false

  let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : unit =
    match stmt with
    | Ast.Proc_def pd -> evaluate_proc_def_stmt pd
    | Ast.Struct s -> failwith "ir.ml: structs are unimplemented"
    | Ast.Let l -> failwith "ir.ml: let statements are unimplemented at the top-level"

  (* --- Entrypoint --- *)

  let generate_inter_lang (program : Ast.program) : string =
    List.iter evaluate_toplvl_stmt program;
    Scope.state.func_section ^ Scope.state.data_section ^ Scope.state.type_section

end

