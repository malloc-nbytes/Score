open Token
open Ast

module Parser = struct
  let expect tokens _type =
    match tokens with
    | [] -> failwith "expect () failed with no tokens"
    | hd :: tl when hd.Token.ttype = _type -> tl, hd
    | hd :: _ ->
       let hdtype = TokenType.to_string hd.ttype
       and ttype = TokenType.to_string _type in
       failwith (Printf.sprintf "expect () failed because acutal <> expected, %s <> %s" hdtype ttype)

  let eat tokens =
    match tokens with
    | [] -> failwith "eat () failed with no tokens"
    | hd :: tl -> tl, hd

  let at tokens =
    match tokens with
    | [] -> None
    | hd :: _ -> Some hd

  let parse_funcstmt tokens = failwith "todo"

  let rec parse_program tokens =
    match tokens with
    | [] -> []
    | hd :: _ when hd.Token.ttype = TokenType.Eof -> []
    | _ -> let rest, funcstmt = parse_funcstmt tokens in
           funcstmt @ parse_program rest
end
