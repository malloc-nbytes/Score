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

module Err = struct
  open Token

  type err_type =
    | Fatal
    | Expect
    | Exhausted_tokens
    | Unknown_token
    | Malformed_proc_def
    | Unreachable
    | Unimplemented
    | Redeclaration
    | Undeclared
    | Syntax
    | Missing_binding
    | Missing_type
    | No_return
    | Type_mismatch

  let err_to_str (err_type : err_type) : string =
    match err_type with
    | Fatal -> "Fatal"
    | Expect -> "Expect"
    | Exhausted_tokens -> "Exhausted_tokens"
    | Unknown_token -> "Unknown_token"
    | Malformed_proc_def -> "Malformed_proc_def"
    | Unreachable -> "Unreachable"
    | Unimplemented -> "Unimplemented"
    | Redeclaration -> "Redeclared Identifier"
    | Undeclared -> "Undeclared Identifier"
    | Syntax -> "Syntax Error"
    | Missing_binding -> "Missing_binding"
    | Missing_type -> "Missing_type"
    | No_return -> "No_return"
    | Type_mismatch -> "Type_mismatch"

  let err (err_type : err_type) (file : string) (func : string)
        ?(msg="") (token : Token.t option) : unit =
    let open Printf in
    let e = err_to_str err_type in
    let failure = e ^ " in " ^ file ^ " " ^ func ^ " ()" in
    let reason = if msg = "" then "N/A" else msg in
    let at, where = match token with
      | Some token ->
         (TokenType.to_string token.ttype) ^ " " ^ token.lexeme, Printf.sprintf "%s:%d:%d:\n" token.fp token.r token.c
      | None -> "N/A", "N/A" in
    Printf.eprintf " ERR\n%s\nReason: %s\nAt: %s\n%s" failure reason at where

  let err_type_mismatch ?(msg="") (t1 : Token.t option) (left_type : TokenType.id_type) (right_type : TokenType.id_type) : unit =
    let open Printf in
    let e = err_to_str Type_mismatch in
    let failure = e in
    let reason = if msg = "" then "N/A" else msg in
    let at, where = match t1 with
      | Some token ->
         (TokenType.to_string token.ttype) ^ " " ^ token.lexeme, Printf.sprintf "%s:%d:%d:" token.fp token.r token.c
      | None -> "None", "None" in
    let lt_str = TokenType.id_type_to_string left_type
    and rt_str = TokenType.id_type_to_string right_type in
    Printf.eprintf " ERR\n%s\nReason: %s\nAt: %s\nTypes: %s ::: %s\n%s\n" failure reason at lt_str rt_str where

end
