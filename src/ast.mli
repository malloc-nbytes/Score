module Ast : sig
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
    | If of if_stmt
    | While of while_stmt
    | Stmt_expr of stmt_expr
    | Ret of ret_stmt
    | Break of Token.t
    | For of for_stmt

  and block_stmt = { stmts : stmt list }

  and ret_stmt = { expr : expr }

  and while_stmt =
    { expr : expr
    ; block : block_stmt
    }

  and for_stmt =
    { init : stmt
    ; cond : expr
    ; after : stmt
    ; block : block_stmt
    }

  and if_stmt =
    { expr : expr
    ; block : block_stmt
    ; else_ : block_stmt option
    }

  and proc_def_stmt =
    { id : Token.t
    ; params : (Token.t * TokenType.id_type) list
    ; block : block_stmt
    ; rettype : TokenType.id_type
    }

  and let_stmt =
    { id : Token.t
    ; type_ : TokenType.id_type
    ; expr : expr
    }

  and mut_stmt =
    { lhs : expr
    ; rhs : expr
    }

  and expr =
    | Binary of binary_expr
    | Term of term_expr
    | Proc_call of proc_call_expr

  and stmt_expr = expr

  and binary_expr =
    { lhs : expr
    ; rhs : expr
    ; op : Token.t
    }

  and term_expr =
    | Ident of Token.t
    | Intlit of Token.t
    | Strlit of Token.t
    | IntCompoundLit of expr list * int

  and proc_call_expr =
    { id : Token.t
    ; args : expr list
    }

end
