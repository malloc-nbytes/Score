module Ast : sig
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

  and node_stmt_compound =
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
    | NodeExprIntLit    of node_expr_int_lit
    | NodeExprStringLit of node_expr_string_lit
    | NodeExprBinop     of node_expr_binop



end
