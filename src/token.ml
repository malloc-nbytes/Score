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

  let rec id_type_to_string = function
    | I32 -> "I32"
    | Str -> "Str"
    | Usize -> "Usize"
    | Char -> "Char"
    | Void -> "Void"
    | Pointer t -> "Pointer " ^ id_type_to_string t
    | Number -> "Number"
    | Array _ -> "Array PRINTING UNIMPLEMENTED"
    | Custom s -> "Custom " ^ s

  (* Convert a Token to a string *)
  let to_string = function
    | Eof -> "Eof"
    | Identifier -> "Identifier"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | StringLiteral -> "StringLiteral"
    | Character -> "Character"
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
    | Type I32 -> "Type I32"
    | Type Usize -> "Usize"
    | Type Char -> "Type Char"
    | Type Str -> "Type Str"
    | Type Void -> "Type Void"
    | Type Pointer t -> "Type Pointer " ^ id_type_to_string t
    | Type Number -> "Number (SHOULD NOT BE USED)"
    | Type (Array _) -> "Type Array PRINTING UNIMPLEMENTED"
    | Type (Custom s) -> "Type Custom " ^ s
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
    | While -> "While"
    | DoubleAmpersand -> "DoubleAmpersand"
    | LessThanEqual -> "LessThanEqual"
    | GreaterThanEqual -> "GreaterThanEqual"
    | NotEqual -> "NotEqual"
    | DoublePipe -> "DoublePipe"
    | Break -> "Break"
    | For -> "For"
    | PlusEquals -> "PlusEquals"
    | MinusEquals -> "MinusEquals"
    | AsteriskEquals -> "AsteriskEquals"
    | ForwardSlashEquals -> "ForwardSlashEquals"
    | PercentEquals -> "PercentEquals"
    | Struct -> "Struct"
    | Ref -> "Ref"
    | Ampersand -> "&"

end

module Token = struct
  type t =
    { lexeme : string
    ; ttype : TokenType.t
    ; r : int
    ; c : int
    ; fp : string
    }

  (* Convert a Token to a string *)
  let to_string token =
    Printf.sprintf "(file: %s, line %d, char %d, %s `%s`)"
      token.fp token.r token.c (TokenType.to_string token.ttype) token.lexeme
end
