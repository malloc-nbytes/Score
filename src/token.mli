module TokenType : sig
  type t =
    | Eof
    | Identifier
    | LParen
    | RParen
    | StringLiteral
    | IntegerLiteral
    | LBrace
    | RBrace
    | Equals
    | Semicolon
    | DoubleEquals
    | DoubleColon
    | Colon
    | RightArrow
    | Comment
    | Type
    | GreaterThan
    | LessThan
    | LBracket
    | RBracket
    | Comma
    | Period
    | Plus
    | Minus
    | Asterisk
    | ForwardSlash
    | Percent
    | Proc
    | Return
    | Let
    | If
    | Else
    | Void
    | While
    | DoubleAmpersand
    | LessThanEqual
    | GreaterThanEqual
    | NotEqual
    | DoublePipe
    | Break

  val to_string : t -> string
end

module Token : sig
  type t =
    { lexeme : string
    ; ttype : TokenType.t
    ; r : int
    ; c : int
    }

  val to_string : t -> string
end
