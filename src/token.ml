module TokenType = struct
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
    | Ret
    | Let

  (* Convert a Token to a string *)
  let to_string = function
    | Eof -> "Eof"
    | Identifier -> "Identifier"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | StringLiteral -> "StringLiteral"
    | IntegerLiteral -> "IntegerLiteral"
    | LBrace -> "LBrace"
    | RBrace -> "RBrace"
    | Equals -> "Equals"
    | Semicolon -> "Semicolon"
    | DoubleEquals -> "DoubleEquals"
    | DoubleColon -> "DoubleColon"
    | Colon -> "Colon"
    | RightArrow -> "RightArrow"
    | Comment -> "Comment"
    | Type -> "Type"
    | GreaterThan -> "GreaterThan"
    | LessThan -> "LessThan"
    | LBracket -> "LBracket"
    | RBracket -> "RBracket"
    | Comma -> "Comma"
    | Period -> "Period"
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Asterisk -> "Asterisk"
    | ForwardSlash -> "ForwardSlash"
    | Percent -> "Percent"
    | Proc -> "Proc"
    | Ret -> "Ret"
    | Let -> "Let"

end

module Token = struct
  type t =
    { value : string
    ; ttype : TokenType.t
    ; r : int
    ; c : int
    }

  (* Convert a Token to a string *)
  let to_string token =
    Printf.sprintf "(line %d, char %d, %s `%s`)"
      token.r token.c (TokenType.to_string token.ttype) token.value
end
