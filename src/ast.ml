open Token

module Ast = struct
  type node_program = node_func list

  and node_func =
    | NodeFuncDeclStmt of node_func_decl_stmt
    | NodeFuncImplStmt of node_func_impl_stmt

  and node_stmt =
    | NodeStmtVarDecl of node_stmt_var_decl
    | NodeStmtVarMut of node_stmt_var_mut

  and node_term =
    | NodeTermID of node_term_id
    | NodeTermIntlit of node_term_intlit

  and node_expr =
    | NodeBinaryExpr of node_bin_expr
    | NodeTerm of node_term

  (* Function Types. *)
  and node_func_decl_stmt =
    { id : string
    ; param_types : Token.t list
    ; ret_type : Token.t
    }

  and node_func_impl_stmt =
    { id : string
    ; decl : node_func_decl_stmt
    ; params : node_function_param list
    ; stmts : node_stmt list
    }

  and node_function_param =
    { id : string
    ; _type : Token.t
    }

  (* Expressions. *)
  and node_term_id = { id : Token.t }

  and node_term_intlit = { intlit : Token.t }

  and node_bin_expr =
    { lhs : node_expr
    ; rhs : node_expr
    ; op : string
    }

  (* Statements. *)
  and node_stmt_var_decl =
    { id : Token.t
    ; expr : node_expr option
    ; constant : bool
    }

  and node_stmt_var_mut =
    { id : Token.t
    ; expr : node_expr
    }
end
