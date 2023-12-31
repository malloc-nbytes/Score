module Lexer = struct
  open Token

  let symbols : (string, TokenType.t) Hashtbl.t = Hashtbl.create 15
  let keywords : (string, TokenType.t) Hashtbl.t = Hashtbl.create 20

  let populate_symbols () =
    let _ = Hashtbl.add symbols "{" TokenType.LBrace in
    let _ = Hashtbl.add symbols "}" TokenType.RBrace in
    let _ = Hashtbl.add symbols "=" TokenType.Equals in
    let _ = Hashtbl.add symbols "+" TokenType.Plus in
    let _ = Hashtbl.add symbols ";" TokenType.Semicolon in
    let _ = Hashtbl.add symbols "::" TokenType.DoubleColon in
    let _ = Hashtbl.add symbols ":" TokenType.Colon in
    let _ = Hashtbl.add symbols "->" TokenType.RightArrow in
    ()
  ;;

  let populate_keywords () =
    let _ = Hashtbl.add keywords "def" TokenType.Def in
    let _ = Hashtbl.add keywords "ret" TokenType.Ret in
    let _ = Hashtbl.add keywords "let" TokenType.Let in
    let _ = Hashtbl.add keywords "i32" TokenType.I32 in
    let _ = Hashtbl.add keywords "void" TokenType.Void in
    ()
  ;;

  let err msg =
    let _ = Printf.printf "[Lexer ERR]: %s\n" msg in
    exit 1
  ;;

  let is_symbol c =
    match Hashtbl.find_opt symbols c with
    | Some t -> Some t
    | None -> None
  ;;

  let is_keyword s =
    match Hashtbl.find_opt keywords s with
    | Some t -> Some t
    | None -> None
  ;;

  let isalpha c =
    let c = int_of_char c in
    (c >= 65 && c <= 90) || (c >= 97 && c <= 122)
  ;;

  let isnum c =
    let c = int_of_char c in
    let c = c - int_of_char '0' in
    (c >= 0) && (c <= 9)
  ;;

  let isalnum c = isalpha c || isnum c ;;

  let isignorable c = c = ' ' || c = '\t' ;;

  let consume_while lst predicate =
    let rec aux lst acc =
      match lst with
      | [] -> acc, []
      | hd :: tl when predicate hd -> aux tl (acc ^ String.make 1 hd)
      | hd :: tl -> acc, hd :: tl
    in
    aux lst ""
  ;;

  let eat lst =
    match lst with
    | [] -> None, []
    | hd :: tl -> Some hd, tl
  ;;

  let peek lst ahead =
    let rec peek' lst i =
      match lst with
      | [] -> None
      | hd :: _ when i = ahead -> Some hd
      | _ :: tl -> peek' tl (i+1)
    in
    peek' lst 1
  ;;

  let lex_file src =
    let rec lex_file' r c lst =
      match lst with
      | [] -> [Token.{value = "Eof"; ttype = Eof}]
      | hd :: tl when hd = '\n' -> lex_file' (r+1) 1 tl
      | hd :: tl when isignorable hd -> lex_file' r (c+1) tl
      | hd :: tl when hd = ':' ->
        (match peek tl 1 with
        | Some ':' ->
            let _, tl' = eat tl in
            Token.{value = "::"; ttype = TokenType.DoubleColon} :: lex_file' r (c+2) tl'
        | _ -> Token.{value = ":"; ttype = TokenType.Colon} :: lex_file' r (c+1) tl)
      | hd :: tl when hd = '"' ->
        let (s, tl') = consume_while tl (fun c -> c <> '"') in
        Token.{value = s; ttype = TokenType.StringLiteral} :: lex_file' r (c+1) (List.tl tl')
      | hd :: tl when isnum hd ->
        let (s, tl') = consume_while tl isnum in
        let s = String.make 1 hd ^ s in
        Token.{value = s; ttype = TokenType.IntegerLiteral} :: lex_file' r (c+1) tl'
      | hd :: tl when hd = '+' -> Token.{value = "+"; ttype = TokenType.Plus} :: lex_file' r (c+1) tl
      | hd :: tl when hd = '*' -> Token.{value = "*"; ttype = TokenType.Asterisk} :: lex_file' r (c+1) tl
      | hd :: tl when hd = '-' ->
        (match peek tl 1 with
        | Some '>' ->
            let _, tl' = eat tl in
            Token.{value = "->"; ttype = TokenType.RightArrow} :: lex_file' r (c+2) tl'
        | _ -> Token.{value = "-"; ttype = TokenType.Minus} :: lex_file' r (c+1) tl)
      | hd :: tl when hd = '/' ->
        (match peek tl 1 with
        | Some '/' ->
            let _, tl' = eat tl in
            let (s, tl'') = consume_while tl' (fun c -> c <> '\n') in
            Token.{value = s; ttype = TokenType.Comment} :: lex_file' r (c+1) tl''
        | _ -> Token.{value = "/"; ttype = TokenType.ForwardSlash} :: lex_file' r (c+1) tl)
      | hd :: tl when hd = ';' -> Token.{value = ";"; ttype = TokenType.Semicolon} :: lex_file' r (c+1) tl
      | hd :: tl ->
        let (s, tl') = consume_while tl (fun c -> isalnum c || c = '_') in
        let s = String.make 1 hd ^ s in
        (match is_keyword s with
        | Some t -> Token.{value = s; ttype = t} :: lex_file' r (c + String.length s) tl'
        | None -> Token.{value = s; ttype = TokenType.Identifier} :: lex_file' r (c + String.length s) tl')
    in
    let _ = populate_symbols () in
    let _ = populate_keywords () in
    lex_file' 1 1 (src |> String.to_seq |> List.of_seq)
  ;;

  let rec print_tokens tokens =
    match tokens with
    | [] -> ()
    | hd :: tl ->
      print_endline (Token.to_string hd);
      print_tokens tl
  ;;
end