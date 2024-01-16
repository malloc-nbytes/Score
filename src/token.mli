module TokenType : sig
  type binop =
    | Plus
    | Minus
    | Asterisk
    | ForwardSlash

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
    (* Keywords *)
    | Def
    | Let
    | Ret
    | Void
    | I32
    (* Other *)
    | Type

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
