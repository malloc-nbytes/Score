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
    | Malformed_func_def
    | Unreachable

  let err_to_str (err_type : err_type) : string =
    match err_type with
    | Fatal -> "Fatal"
    | Expect -> "Expect"
    | Exhausted_tokens -> "Exhausted_tokens"
    | Unknown_token -> "Unknown_token"
    | Malformed_func_def -> "Malformed_func_def"
    | Unreachable -> "Unreachable"

  let err (err_type : err_type) (file : string) (func : string)
        ?(msg="") (token : Token.t option) : unit =
    let s = err_to_str err_type in
    match token with
    | Some token' ->
       let msg = (if msg = "" then msg else "[ERR] " ^ msg ^ "\n") in
       Printf.eprintf
         "[ERR] %s [%s:%s]:\n%s[ERR] conflicting token: %s\n"
         s file func msg (Token.to_string token')
    | None -> Printf.eprintf "[ERR] %s [%s:%s]:\n[ERR] %s\n" s file func msg
  ;;

end
