module TokenType : sig
  type keyword =
    | Proc
    | Ret
    | Let

  type t =
    | Eof
    | Identifier
    | LParen
    | RParen
    | StringLiteral
    | IntegerLiteral
    | Binop
    | LBrace
    | RBrace
    | Equals
    | Semicolon
    | DoubleEquals
    | DoubleColon
    | Colon
    | RightArrow
    | Comment
    | Keyword of keyword
    | Type
    | GreaterThan
    | LessThan
    | LBracket
    | RBracket
    | Comma

  val to_string : t -> string
end

module Token : sig
  type t =
    { value : string
    ; ttype : TokenType.t
    ; r : int
    ; c : int
    }

  val to_string : t -> string
end
