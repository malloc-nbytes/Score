module Utils = struct
  open Token

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
    | _ -> failwith "scr_type_to_bytes: invalid qbe type"

  let scr_to_qbe_type (type_: TokenType.id_type) =
    match type_ with
    | TokenType.I32 -> "w"
    | TokenType.Usize -> "l"
    | TokenType.Str -> "l"
    | TokenType.Char -> "b"
    | TokenType.Number -> "w"
    | TokenType.Pointer TokenType.I32 -> "l"
    | TokenType.Pointer TokenType.Usize -> "l"
    | TokenType.Void -> ""
    | _ -> failwith @@ Printf.sprintf "scr_to_qbe_type: invalid qbe type: %s" (TokenType.id_type_to_string type_)

  let unwrap_ptr (type_: TokenType.id_type) =
    match type_ with
    | TokenType.Pointer t -> t
    | _ -> failwith @@ Printf.sprintf "unwrap_ptr: %s not a pointer" (TokenType.id_type_to_string type_)

end
