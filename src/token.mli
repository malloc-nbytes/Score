module TokenType : sig
  type binop =
    | Plus
    | Minus
    | Asterisk
    | ForwardSlash

  type vartype =
    | I32
    | Str
    | Void

  type keyword =
    | Def
    | Let
    | Ret

  type t =
    | Eof
    | Identifier
    | LParen
    | RParen
    | StringLiteral
    | IntegerLiteral
    | Binop of binop
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
    | Type of vartype

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
