module TokenType = struct
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

  let to_string = function
    | Eof -> "Eof"
    | Identifier -> "Identifier"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | StringLiteral -> "StringLiteral"
    | IntegerLiteral -> "IntegerLiteral"
    | Binop binop -> "Binop"
    | LBrace -> "LBrace"
    | RBrace -> "RBrace"
    | Equals -> "Equals"
    | Semicolon -> "Semicolon"
    | DoubleEquals -> "DoubleEquals"
    | DoubleColon -> "DoubleColon"
    | Colon -> "Colon"
    | RightArrow -> "RightArrow"
    | Comment -> "Comment"
    | Def -> "Def"
    | Let -> "Let"
    | Ret -> "Ret"
    | Void -> "Void"
    | I32 -> "I32"
    | Type -> "Type"

end

module Token = struct
  type t =
    { value : string
    ; ttype : TokenType.t
    ; r : int
    ; c : int
    }

  let to_string token =
    Printf.sprintf "%s: %s" (TokenType.to_string token.ttype) token.value
end
