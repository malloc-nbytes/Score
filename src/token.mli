module TokenType : sig
  type id_type =
    | I32
    | Str
    | Void
    | Array of id_type
    | Custom of string

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
    | Type of id_type
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
    | While
    | DoubleAmpersand
    | LessThanEqual
    | GreaterThanEqual
    | NotEqual
    | DoublePipe
    | Break
    | For
    | PlusEquals
    | MinusEquals
    | AsteriskEquals
    | ForwardSlashEquals
    | PercentEquals

  val to_string : t -> string
  val id_type_to_string : id_type -> string
  val id_types : t list
end

module Token : sig
  type t =
    { lexeme : string
    ; ttype : TokenType.t
    ; r : int
    ; c : int
    ; fp : string
    }

  val to_string : t -> string
end
