module TokenType : sig
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
    | Plus
    | Minus
    | Asterisk
    | ForwardSlash
    | DoubleColon
    | Colon
    | RightArrow
    | Comment
    (* Keywords *)
    | Def
    | Let
    | Ret
    | Void
    | I32

  val to_string : t -> string
end

module Token : sig
  type t =
    { value : string
    ; ttype : TokenType.t
    }

  val to_string : t -> string
end
