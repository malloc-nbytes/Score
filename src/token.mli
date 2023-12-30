module TokenType : sig
  type t =
    | Eof
    | LParen
    | RParen
    | StringLiteral
    | IntegerLiteral
    | Keyword
    | Binop

  val to_string : t -> string
end

module Token : sig
  type t =
    { value : string
    ; ttype : TokenType.t
    }

  val to_string : t -> string
end
