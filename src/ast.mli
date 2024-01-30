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

  val ast_dump : program -> unit

end
