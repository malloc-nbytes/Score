(* MIT License

   * Copyright (c) 2023 malloc-nbytes

   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:

   * The above copyright notice and this permission notice shall be included in all
   * copies or substantial portions of the Software.

   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   * SOFTWARE. *)

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
    | Return
    | Let
    | If
    | Else
    | Void
    | While
    | DoubleAmpersand

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
    | Return -> "Return"
    | Let -> "Let"
    | If -> "If"
    | Else -> "Else"
    | Void -> "Void"
    | While -> "While"
    | DoubleAmpersand -> "DoubleAmpersand"

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
