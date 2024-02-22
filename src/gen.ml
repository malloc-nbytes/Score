module Gen = struct
  open Ast
  open Printf

  let func_section = ref ""
  let data_section = ref ""

  let evaluate_expr (expr : Ast.expr) : unit =
    match expr with
    | Ast.Binary bin -> assert false
    | Ast.Term Ast.Ident ident -> assert false
    | Ast.Term Ast.Intlit term -> assert false
    | Ast.Proc_call pc -> assert false

  let evaluate_mut_stmt (stmt : Ast.mut_stmt) : unit =
    assert false

  let evaluate_let_stmt (stmt : Ast.let_stmt) : unit =
    assert false

  let evaluate_block_stmt (stmt : Ast.block_stmt) : unit =
    func_section := sprintf "%s    ret 0\n" !func_section

  let evaluate_proc_def_stmt (stmt : Ast.proc_def_stmt) : unit =
    func_section :=
      sprintf "%sexport function w $%s() {\n@start\n" !func_section stmt.id.value;
    evaluate_block_stmt stmt.block;
    func_section := sprintf "%s}\n" !func_section

  let evaluate_stmt (stmt : Ast.stmt) : unit =
    match stmt with
    | Ast.Proc_def procdef -> assert false
    | Ast.Block block -> assert false
    | Ast.Let letstmt -> assert false
    | Ast.Mut mutstmt -> assert false
    | Ast.If ifstmt -> assert false
    | Ast.While whilestmt -> assert false
    | Ast.Stmt_expr se -> assert false

  let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : unit =
    match stmt with
    | Ast.Proc_def s -> evaluate_proc_def_stmt s
    | Ast.Let s -> evaluate_let_stmt s

  let generate_inter_lang (program : Ast.program) : string =
    List.iter evaluate_toplvl_stmt program;
    !func_section ^ !data_section

end

