module TokenType = struct
  type t =
    | Eof
    | LParen
    | RParen
    | StringLiteral
    | IntegerLiteral
    | Keyword
    | Binop

  let to_string = function
    | Eof -> "Eof"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | StringLiteral -> "StringLiteral"
    | IntegerLiteral -> "IntegerLiteral"
    | Keyword -> "Keyword"
    | Binop -> "Binop"
end

module Token = struct
  type t =
    { value : string
    ; ttype : TokenType.t
    }

  let to_string token =
    Printf.sprintf "%s: %s" (TokenType.to_string token.ttype) token.value
end
