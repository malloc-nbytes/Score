module TokenType = struct
  type t =
    | Eof
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
    (* Keywords *)
    | Def
    | Let
    | Ret
    | Void
    | I32

  let to_string = function
    | Eof -> "Eof"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | StringLiteral -> "StringLiteral"
    | IntegerLiteral -> "IntegerLiteral"
    | Binop -> "Binop"
    | LBrace -> "LBrace"
    | RBrace -> "RBrace"
    | Equals -> "Equals"
    | Semicolon -> "Semicolon"
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Asterisk -> "Asterisk"
    | ForwardSlash -> "ForwardSlash"
    | DoubleColon -> "DoubleColon"
    | Colon -> "Colon"
    | RightArrow -> "RightArrow"
    | Def -> "Def"
    | Let -> "Let"
    | Ret -> "Ret"
    | Void -> "Void"
    | I32 -> "I32"
end

module Token = struct
  type t =
    { value : string
    ; ttype : TokenType.t
    }

  let to_string token =
    Printf.sprintf "%s: %s" (TokenType.to_string token.ttype) token.value
end
