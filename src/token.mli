module TokenType : sig
  type id_type =
    | I32
    | Str
    | Usize
    | Char
    | Void
    | Number (* NOT TO BE USED IN PARSING *)
    | Pointer of id_type
    | Array of id_type * (int option)
    | Custom of string

  type t =
    | Eof
    | Identifier
    | LParen
    | RParen
    | StringLiteral
    | Character
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
    | Struct
    | Ref
    | Ampersand

  val to_string : t -> string
  val id_type_to_string : id_type -> string
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
