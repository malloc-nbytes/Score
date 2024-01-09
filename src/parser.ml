open Token
open Ast

module Parser = struct
  let expect tokens _type =
    match tokens with
    | [] -> failwith "expect () failed with no tokens"
    | hd :: tl when hd.Token.ttype = _type -> hd, tl
    | hd :: _ ->
       let hdtype = TokenType.to_string hd.ttype
       and ttype = TokenType.to_string _type in
       failwith (Printf.sprintf "expect () failed because acutal <> expected, %s <> %s" hdtype ttype)
  ;;

  let eat tokens =
    match tokens with
    | [] -> failwith "eat () failed with no tokens"
    | hd :: tl -> tl, hd
  ;;

  let at tokens =
    match tokens with
    | [] -> None
    | hd :: _ -> Some hd
  ;;

  let rec parse_func_impl tokens = failwith "todo"

  (* def sum :: i32 -> i32 -> i32 *)
  let rec parse_func_def (tokens : Token.t list) =
    let _, tokens = expect tokens TokenType.Def in
    let name, tokens = expect tokens TokenType.Identifier in

    let rec parse_func_args (tokens : Token.t list) (acc : Token.t list) =
      match tokens with
      | [] -> failwith "parse_func_args () failed with no tokens"
      | hd :: tl when hd.Token.ttype = TokenType.RightArrow || hd.Token.ttype = TokenType.DoubleColon ->
         let _type, rest = expect tl TokenType.Type in
         parse_func_args rest (acc @ [_type])
      | _ -> acc, tokens
    in

    let _types, rest = parse_func_args tokens [] in

    let _ = print_endline name.value in
    let _ = List.iter (fun t -> Printf.printf "%s\n" (Token.to_string t)) _types in

    failwith "todo"
  ;;

  let parse_funcstmt tokens =
    match tokens with
    | [] -> failwith "parse_funcstmt () failed with no tokens"
    | hd :: tl when hd.Token.ttype = TokenType.Def ->
       let _ = parse_func_def tokens in
       failwith "todo"
    | hd :: tl when hd.Token.ttype = TokenType.Identifier -> failwith "todo"
    | _ -> failwith "unreachable"
  ;;

  let rec parse_program tokens =
    match tokens with
    | [] -> []
    | hd :: _ when hd.Token.ttype = TokenType.Eof -> []
    | _ ->
       let funcstmt, tokens' = parse_funcstmt tokens in
       funcstmt :: parse_program tokens'
  ;;
end
