module Gen = struct
  open Ast

  let asm = ref ""

  let evaluate_expr (expr : Ast.expr) : unit =
    match expr with
    | Ast.Binary bin -> assert false
    | Ast.Term Ast.Ident ident -> assert false
    | Ast.Term Ast.Intlit term -> assert false

  let evaluate_mut_stmt (stmt : Ast.mut_stmt) : unit =
    assert false

  let evaluate_let_stmt (stmt : Ast.let_stmt) : unit =
    assert false

  let evaluate_block_stmt (stmt : Ast.block_stmt) : unit =
    assert false

  let evaluate_proc_def_stmt (stmt : Ast.proc_def_stmt) : unit =
    assert false

  let evaluate_stmt (stmt : Ast.stmt) : unit =
    match stmt with
    | Ast.Proc_def procdef -> assert false
    | Ast.Block block -> assert false
    | Ast.Let letstmt -> assert false
    | Ast.Mut mutstmt -> assert false
    | Ast.If ifstmt -> assert false
    | Ast.While whilestmt -> assert false

  let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : unit =
    match stmt with
    | Ast.Proc_def stmt' -> assert false
    | Ast.Let stmt' -> assert false

  let generate_bytecode (program : Ast.program) : string =
    let rec aux = function
      | [] -> !asm
      | hd :: tl -> let _ = evaluate_toplvl_stmt hd in aux tl in
    let _ = aux program in
    aux program

end
