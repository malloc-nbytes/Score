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

open Token

let compat_types =
  let open TokenType in
  let tbl = Hashtbl.create 10 in
  let _ = Hashtbl.add tbl I32 [I32; Usize]
  and _ = Hashtbl.add tbl Usize [Usize; I32]
  and _ = Hashtbl.add tbl Str [Str]
  and _ = Hashtbl.add tbl Char [Char]
  and _ = Hashtbl.add tbl Bool [Bool]
  and _ = Hashtbl.add tbl Void [Void] in
  tbl

let rec typecheck ty1 ty2 =
  let open TokenType in
  match ty1, ty2 with
  | Pointer p1, Pointer p2 -> typecheck p1 p2
  | Array (a1, len1), Array (a2, len2) -> typecheck a1 a2 && len1 = len2
  | Custom c1, Custom c2 -> c1 = c2
  | _ ->
     let rec aux = function
       | [] -> false
       | hd :: _ when hd = ty2 -> true
       | _ :: tl -> aux tl in
     aux (match Hashtbl.find_opt compat_types ty1 with
          | Some compatible_types -> compatible_types
          | None -> [])

let rec typecheck_for_casting ty1 ty2 =
  let open TokenType in
  match ty1, ty2 with
  | Pointer p1, Pointer p2 -> true
  | _ -> typecheck ty1 ty2

