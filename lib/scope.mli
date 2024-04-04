open Token

type variable =
  { id : Token.t
  ; type_ : TokenType.id_type
  }

type procedure =
  { id : Token.t
  ; params : (Token.t * TokenType.id_type) list
  ; rettype : TokenType.id_type
  }

type structure =
  { id : Token.t
  (* name * type * offset *)
  ; members : (Token.t * TokenType.id_type * int) list
  ; size : int
  }
