module Ast = struct
  open Token

  type program = toplvl_stmt list

  and toplvl_stmt =
    | Proc_def of proc_def_stmt
    | Let of let_stmt

  and stmt =
    | Proc_def of proc_def_stmt
    | Block of block_stmt
    | Let of let_stmt
    | Mut of mut_stmt

  and proc_def_stmt =
    { id : Token.t
    ; params : (Token.t * TokenType.t) list
    ; block : block_stmt
    ; rettype : TokenType.t
    }

  and let_stmt =
    { id : Token.t
    ; type_ : Token.t
    ; expr : expr
    }

  and mut_stmt =
    { id : Token.t
    ; expr : expr
    }

  and block_stmt = { stmts : stmt list}

  and expr =
    | Binary of binary_expr
    | Term of term_expr

  and binary_expr =
    { lhs : expr
    ; rhs : expr
    ; op : Token.t
    }

  and term_expr =
    | Ident of Token.t
    | Intlit of Token.t

  let ast_dump (program : program) : unit =
    let open Printf in

    let indent (d : int) : string =
      let s = ref "" in
      let _ = for i = 1 to d do s := !s ^ "  "
              done in !s in

    let rec expr_dump (expr : expr) (depth : int) : unit =
      let spaces = indent depth in
      match expr with
      | Binary b ->
         let _ = expr_dump b.lhs (depth + 1) in
         let _ = printf "%s%s\n" spaces b.op.value in
         expr_dump b.rhs (depth + 1)
      | Term Ident i -> printf "%s%s\n" spaces i.value
      | Term Intlit t -> printf "%s%s\n" spaces t.value in

    let mut_stmt_dump (stmt : mut_stmt) (depth : int) : unit =
      let spaces = indent depth in
      let _ = printf "%s%s = \n" spaces stmt.id.value in
      expr_dump stmt.expr (depth + 1) in

    let let_stmt_dump (stmt : let_stmt) (depth : int) : unit =
      let spaces = indent depth in
      let _ = printf "%sLET %s = " spaces stmt.id.value in
      expr_dump stmt.expr (depth + 1) in

    let rec block_stmt_dump (stmt : block_stmt) (depth : int) : unit =
      match stmt.stmts with
      | [] -> ()
      | hd :: tl ->
         (match hd with
          | Proc_def p -> assert false
          | Block b -> assert false
          | Let l -> assert false
          | Mut m -> assert false) in

    let proc_def_stmt_dump (stmt : proc_def_stmt) (depth : int) : unit =
      assert false in

    let toplvl_stmt_dump (stmt : toplvl_stmt) : unit =
      match stmt with
      | Proc_def s -> assert false
      | Let s -> assert false in
    List.iter toplvl_stmt_dump program

end
