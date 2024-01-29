module Ast : sig
  open Token

  type program = stmt list

  and stmt =
    | Proc_def of proc_def
    | Block of block
    | Let of let_
    | Mut of mut

  and proc_def =
    { name : Token.t
    ; params : (Token.t * TokenType.t) list
    ; block : block
    }

  and let_ =
    { id : Token.t
    ; expr : expr
    }

  and mut =
    { id : Token.t
    ; expr : expr
    }

  and block = { stmts : stmt list}

  and expr =
    | Binary of binary
    | Term of term

  and binary =
    { lhs : expr
    ; rhs : expr
    ; op : Token.t
    }

  and term =
    | Ident of Token.t
    | Intlit of Token.t

end
