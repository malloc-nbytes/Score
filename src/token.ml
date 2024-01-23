module TokenType = struct
  type binop =
    | Plus
    | Minus
    | Asterisk
    | ForwardSlash
    | Percent

  type vartype =
    | I32
    | Str
    | Void

  type keyword =
    | Proc
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
    | GreaterThan
    | LessThan
    | LBracket
    | RBracket

  (* Convert a Token to a string *)
  let to_string = function
    | Eof -> "Eof"
    | Identifier -> "Identifier"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | StringLiteral -> "StringLiteral"
    | IntegerLiteral -> "IntegerLiteral"
    | Binop _ -> "Binop"
    | LBrace -> "LBrace"
    | RBrace -> "RBrace"
    | Equals -> "Equals"
    | Semicolon -> "Semicolon"
    | DoubleEquals -> "DoubleEquals"
    | DoubleColon -> "DoubleColon"
    | Colon -> "Colon"
    | RightArrow -> "RightArrow"
    | Comment -> "Comment"
    | Type _ -> "type"
    | Keyword _ -> "keyword"
    | GreaterThan -> "GreaterThan"
    | LessThan -> "LessThan"
    | LBracket -> "LBracket"
    | RBracket -> "RBracket"
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
    Printf.sprintf "%s: %s (r: %d, c: %d)" (TokenType.to_string token.ttype) token.value token.r token.c
end
