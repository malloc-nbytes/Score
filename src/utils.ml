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

  let scr_type_to_bytes = function
    | TokenType.I32 -> "4"
    | TokenType.Usize -> "8"
    | TokenType.Str -> "8"
    | TokenType.Char -> "1"
    | TokenType.Pointer _ -> "4"
    | _ -> failwith "scr_type_to_bytes: invalid qbe type"

  let scr_to_qbe_type = function
    | TokenType.I32 -> "w"
    | TokenType.Usize -> "l"
    | TokenType.Str -> "l"
    | TokenType.Char -> "b"
    | TokenType.Number -> "w"
    | TokenType.Pointer TokenType.I32 -> "l"
    | TokenType.Pointer TokenType.Usize -> "l"
    | _ -> failwith "scr_to_qbe_type: invalid type"

end
