module Ast = struct
  open Token

  type node_prog =
    { stmts : node_stmt list
    }

  and node_stmt =
    | NodeStmtFuncDef  of node_stmt_func_def
    | NodeStmtFuncCall of node_stmt_func_call
    | NodeStmtCompound of node_stmt_compound
    | NodeStmtLet      of node_stmt_let
    | NodeStmtMut      of node_stmt_mut

  and node_stmt_func_def =
    { id : string
    ; params : (string * TokenType.t) list
    ; rtype : Token.t
    ; compound_stmt : node_stmt_compound
    }

  and node_stmt_compound =
    { stmts : node_stmt list
    }

  and node_stmt_func_call =
    { id : string
    ; args : node_expr list
    }

  and node_stmt_let =
    { id : string
    ; expr : node_expr
    ; mut : bool
    }

  and node_stmt_mut =
    { id : string
    ; expr : node_expr
    }

  and node_expr =
    | NodeBinExpr of node_bin_expr
    | NodeTerm    of node_term

  and node_term =
    | NodeTermID     of node_term_id
    | NodeTermIntLit of node_term_intlit

  and node_bin_expr =
    { lhs : node_expr
    ; rhs : node_expr
    ; op : string
    }

  and node_term_id =
    { id : Token.t
    }

  and node_term_intlit =
    { intlit : Token.t
    }

  let dump_ast (prog : node_prog) =
    let open Printf in

    let indent d = let s = ref "" in for i = 1 to d do s := !s ^ "  " done; !s in

    let rec dump_expr (expr : node_expr) (depth : int) : unit =
      let spaces = indent depth in
      match expr with
      | NodeBinExpr e ->
         printf "%slhs:\n" spaces;
         dump_expr e.lhs (depth+1);
         printf "%s%s\n" spaces e.op;
         printf "%srhs:\n" spaces;
         dump_expr e.rhs (depth+1)
      | NodeTerm NodeTermID e -> printf "%s%s\n" spaces e.id.value
      | NodeTerm NodeTermIntLit e -> printf "%s%s\n" spaces e.intlit.value
    in

    let rec dump_compound_stmt (stmt : node_stmt_compound) (depth : int) : unit =
      let spaces = indent depth in
      List.iter (fun ns ->
          match ns with
          | NodeStmtFuncDef ns' -> dump_node_stmt_func_def ns' (depth+1)
          | NodeStmtFuncCall ns' -> failwith "NodeStmtFuncCall unimplemented"
          | NodeStmtCompound ns' -> dump_compound_stmt ns' (depth+1)
          | NodeStmtLet ns' -> printf "%sLET %s =\n" spaces ns'.id; dump_expr ns'.expr (depth+1)
          | NodeStmtMut ns' -> failwith "NodeStmtMut unimplemented"
        ) stmt.stmts

    and dump_node_stmt_func_def (stmt : node_stmt_func_def) (depth : int) : unit =
      let spaces = indent depth in
      printf "%s%s (" spaces stmt.id;

      List.iter (fun p -> printf "%s%s," spaces @@ fst p) stmt.params;
      printf "%s)\n" spaces;

      dump_compound_stmt stmt.compound_stmt (depth+1)
    in

    List.iter (fun s ->
        match s with
        | NodeStmtFuncDef s' -> dump_node_stmt_func_def s' 0
        | _ -> failwith "unimplemented"
      ) prog.stmts
  ;;

end
