open Token

module Ast : sig
  type node_func_stmt =
    { id : string
    ; _type : Token.t
    ; params : node_function_param list
    ; stmts : (node_stmt list) option
    }

  and node_function_param =
    { id : string
    ; _type : Token.t
    }

  and node_term_id = { id : Token.t }

  and node_term_intlit = { intlit : Token.t }

  and node_bin_expr =
    { lhs : node_expr
    ; rhs : node_expr
    ; op : string
    }

  and node_stmt_var_decl =
    { id : Token.t
    ; expr : node_expr option
    ; constant : bool
    }

  and node_stmt_var_mut =
    { id : Token.t
    ; expr : node_expr
    }

  and node_stmt =
    | NodeStmtVarDecl of node_stmt_var_decl
    | NodeStmtVarMut of node_stmt_var_mut

  and node_term =
    | NodeTermID of node_term_id
    | NodeTermIntlit of node_term_intlit

  and node_expr =
    | NodeBinaryExpr of node_bin_expr
    | NodeTerm of node_term
end
