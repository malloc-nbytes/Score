open Token

module Ast = struct
  type node_prog = { stmts : node_stmt list }
  and node_stmt =
    | NodeStmtVarDecl
    | NodeStmtVarMut
  and node_binary_expr =
    { lhs : node_expr
    ; rhs : node_expr
    ; op : string
    }
  and node_term_id = { id : Token.t }
  and node_term_intlit = { intlit : Token.t }
  and node_bin_expr =
    { lhs : node_expr
    ; rhs : node_expr
    ; op : string
    }
  and node_term =
    | NodeTermID of node_term_id
    | NodeTermIntlit of node_term_intlit
  and node_expr =
    | NodeBinaryExpr of node_bin_expr
    | NodeTerm of node_term
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
