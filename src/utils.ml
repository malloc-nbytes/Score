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

module Utils = struct
  open Token
  open Scope

  let unwrap = function
    | Some k -> k
    | None -> failwith "unwrap: tried to unwrap a None value"

  let file_to_str filepath =
    let ch = open_in_bin filepath in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

  let write_to_file filepath content =
    let oc = open_out filepath in
    let _ = Printf.fprintf oc "%s" content in
    close_out oc

  let scr_type_to_bytes (type_: TokenType.id_type) =
    match type_ with
    | TokenType.I32 -> "4"
    | TokenType.Usize -> "8"
    | TokenType.Str -> "8"
    | TokenType.Char -> "1"
    | TokenType.Pointer _ -> "8"
    | TokenType.Number -> "4"
    | TokenType.Array (TokenType.Str, Some len) -> Printf.sprintf "%d" (8 * len)
    | TokenType.Array (TokenType.I32, Some len) -> Printf.sprintf "%d" (4 * len)
    | TokenType.Array (TokenType.Char, Some len) -> Printf.sprintf "%d" (1 * len)
    | TokenType.Array (_, Some len) -> Printf.sprintf "%d" (8 * len)
    | TokenType.Array (_, None) -> "8"
    | TokenType.Custom (id) -> string_of_int ((Scope.get_struct_from_tbl id).size)
    | _ -> failwith @@ Printf.sprintf "scr_type_to_bytes: invalid type: %s" (TokenType.id_type_to_string type_)

  let scr_to_qbe_type (type_: TokenType.id_type) =
    match type_ with
    | TokenType.I32 -> "w"
    | TokenType.Usize -> "l"
    | TokenType.Str -> "l"
    | TokenType.Char -> "b" (* NOTE: was originally `b` *)
    | TokenType.Number -> "w"
    | TokenType.Pointer TokenType.I32 -> "l"
    | TokenType.Pointer TokenType.Usize -> "l"
    | TokenType.Pointer TokenType.Str -> "l"
    | TokenType.Pointer TokenType.Char -> "l"
    | TokenType.Void -> ""
    | TokenType.Array (t, _) -> "l"
    | TokenType.Custom _ -> "l"
    | TokenType.Pointer TokenType.Custom _ -> "l"
    | _ -> failwith @@ Printf.sprintf "scr_to_qbe_type: invalid qbe type: %s" (TokenType.id_type_to_string type_)

  let unwrap_array (type_: TokenType.id_type) =
    match type_ with
    | TokenType.Array (t, _) -> t
    | TokenType.Str -> type_
    | _ -> failwith @@ Printf.sprintf "unwrap_array: %s not an array" (TokenType.id_type_to_string type_)

  let unwrap_ptr (type_: TokenType.id_type) =
    match type_ with
    | TokenType.Pointer t -> t
    | _ -> failwith @@ Printf.sprintf "unwrap_ptr: %s not a pointer" (TokenType.id_type_to_string type_)

end
