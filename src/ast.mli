module Ast : sig
  open Token

  type node_prog =
    { stmts : node_stmt list
    }

  and node_stmt =
    | NodeStmtFuncDef  of node_stmt_func_def
    | NodeStmtFuncCall of node_stmt_func_call
    | NodeStmtBlock of node_stmt_block
    | NodeStmtLet      of node_stmt_let
    | NodeStmtMut      of node_stmt_mut

  and node_stmt_func_def =
    { id : string
    ; params : (string * TokenType.t) list
    ; rtype : Token.t
    ; block_stmt : node_stmt_block
    }

  and node_stmt_func_call =
    { id : string
    ; args : node_expr list
    }

  and node_stmt_block =
    { stmts : node_stmt list
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

  val dump_ast : node_prog -> unit

end
