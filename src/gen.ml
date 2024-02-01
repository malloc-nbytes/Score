module Gen = struct
  open Ast
  open Opcode

  let evaluate_expr (expr : Ast.expr) : Opcode.byte list =
    match expr with
    | Ast.Binary bin -> assert false
    | Ast.Term Ast.Ident ident -> failwith "Ident Term expr not implemented"
    | Ast.Term Ast.Intlit term ->
       let value = int_of_string term.value
       and bytecode = Opcode.get_instr Opcode.IPush in
       [bytecode; value]

  let evaluate_mut_stmt (stmt : Ast.mut_stmt) : Opcode.byte =
    assert false

  let evaluate_let_stmt (stmt : Ast.let_stmt) : Opcode.byte =
    assert false

  let evaluate_block_stmt (stmt : Ast.block_stmt) : Opcode.byte list =
    assert false

  let evaluate_proc_def_stmt (stmt : Ast.proc_def_stmt) : Opcode.byte list =
    assert false

  let evaluate_stmt (stmt : Ast.stmt) : Opcode.byte list =
    match stmt with
    | Ast.Proc_def procdef -> evaluate_proc_def_stmt procdef
    | Ast.Block block -> assert false
    | Ast.Let letstmt -> assert false
    | Ast.Mut mutstmt -> assert false
    | Ast.If ifstmt -> failwith "traversing if_stmt not implemented"
    | Ast.While whilestmt -> failwith "traversing while_stmt not implemented"

  let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : Opcode.byte list =
    match stmt with
    | Ast.Proc_def stmt' -> assert false
    | Ast.Let stmt' -> assert false

  let generate_bytecode (program : Ast.program) : Opcode.byte list =
    let rec aux (stmts : Ast.toplvl_stmt list) : Opcode.byte list =
      match stmts with
      | [] -> []
      | hd :: tl -> evaluate_toplvl_stmt hd @ aux tl in
    aux program

end
