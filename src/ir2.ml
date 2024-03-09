module Ir2 = struct
  open Printf
  open Ast
  open Token
  open Err
  open Scope

  let func_section = ref ""
  let data_section = ref ""
  let type_section = ref ""

  (* --- Helpers --- *)

  let scr_to_qbe_type = function
    | TokenType.I32 -> "w"
    | TokenType.Str -> "l"
    | TokenType.Char -> "b"
    | _ -> failwith "scr_to_qbe_type: invalid type"

  (* --- Emissions --- *)

  let emit_proc_def
        (export : bool)
        (procname : string)
        (params : (Token.t * TokenType.id_type) list)
        (rettype : TokenType.id_type) : unit =
    let emitted_procname = procname
    and emitted_params = List.fold_left (fun acc param ->
                      let id = (fst param).Token.lexeme in
                      let type_ = scr_to_qbe_type @@ snd param in
                      acc ^ type_ ^ " %" ^ id ^ ", "
                    ) "" params
    and emitted_export = if export then "export" else ""

    and emitted_rettype = scr_to_qbe_type rettype in
    func_section :=
      sprintf "%s%s function %s $%s(%s) {\n@start\n"
        !func_section emitted_export emitted_rettype emitted_procname emitted_params

  let __emit_instr (id : string) (type_ : TokenType.id_type) (rhs : string) (instr : string) : unit =
    let emitted_id = id
    and emitted_type = scr_to_qbe_type type_ in
    func_section :=
      sprintf "%s    %%%s =%s %s %s\n" !func_section emitted_id emitted_type instr rhs

  let emit_closing_brace () : unit =
    func_section := sprintf "%s}\n" !func_section

  let emit_copy (id : string) (type_ : TokenType.id_type) (rhs : string) : unit =
    __emit_instr id type_ rhs "copy"

  let emit_extsb (id : string) (type_ : TokenType.id_type) (rhs : string) : unit =
    __emit_instr id type_ rhs "extsb"

  let emit_extsw (id : string) (type_ : TokenType.id_type) (rhs : string) : unit =
    __emit_instr id type_ rhs "extsw"

  let emit_assignment (lhs : string) (type_ : TokenType.id_type) (rhs : string) : unit =
    let emitted_type = scr_to_qbe_type type_ in
    func_section := sprintf "%s    %s =%s %s\n" !func_section lhs emitted_type rhs

  (* --- Evaluations --- *)

  let rec evaluate_expr (expr : Ast.expr) : string =
    match expr with
    | Ast.Binary bin -> assert false
    | Ast.Array_retrieval ar -> assert false
    | Ast.Term Ast.Ident ident -> assert false
    | Ast.Term Ast.Intlit intlit -> assert false
    | Ast.Term Ast.Char chara -> assert false
    | Ast.Term Ast.Strlit strlit -> assert false
    | Ast.Term (Ast.IntCompoundLit (exprs, len)) -> assert false
    | Ast.Proc_call pc -> assert false

  let evaluate_let_stmt (stmt : Ast.let_stmt) : unit =
    let id = stmt.id
    and type_ = stmt.type_
    and rhs = evaluate_expr stmt.expr in
    emit_copy id.lexeme type_ rhs

  let rec evaluate_block_stmt (bs : Ast.block_stmt) : unit =
    List.iter evaluate_stmt bs.stmts

  and evaluate_proc_def_stmt (pd : Ast.proc_def_stmt) : unit =
    emit_proc_def true pd.id.lexeme pd.params pd.rettype;
    evaluate_block_stmt pd.block;
    emit_closing_brace ()

  and evaluate_stmt = function
    | Ast.Proc_def pd -> assert false
    | Ast.Block b -> assert false
    | Ast.Let l -> evaluate_let_stmt l
    | Ast.Mut m -> assert false
    | Ast.If i -> assert false
    | Ast.While w -> assert false
    | Ast.Stmt_expr se -> assert false
    | Ast.Ret r -> assert false
    | Ast.Break b -> assert false
    | Ast.For f -> assert false

  let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : unit =
    match stmt with
    | Ast.Proc_def pd -> evaluate_proc_def_stmt pd
    | Ast.Struct s -> failwith "ir.ml: structs are unimplemented"
    | Ast.Let l -> failwith "ir.ml: let statements are unimplemented at the top-level"

  (* --- Entrypoint --- *)

  let generate_inter_lang (program : Ast.program) : string =
    List.iter evaluate_toplvl_stmt program;
    !func_section ^ !data_section ^ !type_section

end
