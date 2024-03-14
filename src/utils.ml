module Utils = struct
  open Token

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
    | TokenType.Number -> "8"
    | TokenType.Array (TokenType.I32, Some len) -> Printf.sprintf "%d" (4 * len)
    | _ -> failwith @@ Printf.sprintf "scr_type_to_bytes: invalid type: %s" (TokenType.id_type_to_string type_)

  let scr_to_qbe_type (type_: TokenType.id_type) =
    match type_ with
    | TokenType.I32 -> "w"
    | TokenType.Usize -> "l"
    | TokenType.Str -> "l"
    | TokenType.Char -> "b"
    | TokenType.Number -> "l"
    | TokenType.Pointer TokenType.I32 -> "l"
    | TokenType.Pointer TokenType.Usize -> "l"
    | TokenType.Void -> ""
    | TokenType.Array (t, _) -> "l"
    | _ -> failwith @@ Printf.sprintf "scr_to_qbe_type: invalid qbe type: %s" (TokenType.id_type_to_string type_)

  let unwrap_array (type_: TokenType.id_type) =
    match type_ with
    | TokenType.Array (t, _) -> t
    | _ -> failwith @@ Printf.sprintf "unwrap_array: %s not an array" (TokenType.id_type_to_string type_)

  let unwrap_ptr (type_: TokenType.id_type) =
    match type_ with
    | TokenType.Pointer t -> t
    | _ -> failwith @@ Printf.sprintf "unwrap_ptr: %s not a pointer" (TokenType.id_type_to_string type_)

end
