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

module TokenType : sig
  type id_type =
    | I32
    | Str
    | Usize
    | Char
    | Bool
    | Void
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
    | Import
    | Export
    | Def
    | Macro
    | End
    | In
    | Null
    | StructIdentifier
    | Module
    | Where
    | True
    | False
    | Underscore
    | TriplePeriod
    | Extern

  val to_string : t -> string
  val string_of_id_type : id_type -> string
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
