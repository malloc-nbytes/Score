module Lexer = struct
  open Token

  let keywords : (string, TokenType.t) Hashtbl.t = Hashtbl.create 20

  let populate_keywords () =
    let _ = Hashtbl.add keywords "def" (Keyword TokenType.Def) in
    let _ = Hashtbl.add keywords "ret" (Keyword TokenType.Ret) in
    let _ = Hashtbl.add keywords "let" (Keyword TokenType.Let) in
    let _ = Hashtbl.add keywords "i32" (Type TokenType.I32) in
    let _ = Hashtbl.add keywords "void" (Type TokenType.Void) in
    ()
  ;;

  let err msg =
    let _ = Printf.printf "[Lexer ERR]: %s\n" msg in
    exit 1
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

  let isalnum c = isalpha c || isnum c;;

  let consume_while (lst : char list) (predicate : char -> bool) : string * char list =
    let rec aux lst acc =
      match lst with
      | [] -> acc, []
      | hd :: tl when predicate hd -> aux tl (acc ^ String.make 1 hd)
      | hd :: tl -> acc, hd :: tl
    in
    aux lst ""
  ;;

  let rec lex_file (src : char list) (r : int) (c : int) : Token.t list =
    match src with
    | []               -> [Token.{value = "Eof"; ttype = TokenType.Eof; r; c}]
    | '\n' :: tl       -> lex_file tl r (c+1)
    | '\t' :: tl       -> lex_file tl r (c+1)
    | ' ' :: tl        -> lex_file tl r (c+1)
    | '/' :: '/' :: tl -> let comment, rest = consume_while tl (fun c -> c = '\n') in
                          [Token.{value = comment; ttype = Comment; r; c = c+2+(String.length comment)}]
                          @ lex_file tl r (c+2+String.length comment)
    | '"' :: tl        -> let strlit, rest = consume_while tl (fun c -> c = '"') in
                          [Token.{value = strlit; ttype = StringLiteral; r; c = c+2+(String.length strlit)}]
                          @ lex_file (List.tl tl) r (c+2+String.length strlit)
    | ':' :: ':' :: tl -> [Token.{value = "::"; ttype = DoubleColon; r; c}] @ lex_file tl r (c+2)
    | '-' :: '>' :: tl -> [Token.{value = "->"; ttype = RightArrow; r; c}] @ lex_file tl r (c+2)
    | '(' :: tl        -> [Token.{value = "("; ttype = LParen; r; c}] @ lex_file tl r (c+1)
    | ')' :: tl        -> [Token.{value = ")"; ttype = RParen; r; c}] @ lex_file tl r (c+1)
    | '{' :: tl        -> [Token.{value = "{"; ttype = LBrace; r; c}] @ lex_file tl r (c+1)
    | '}' :: tl        -> [Token.{value = "}"; ttype = RBrace; r; c}] @ lex_file tl r (c+1)
    | ';' :: tl        -> [Token.{value = ";"; ttype = Semicolon; r; c}] @ lex_file tl r (c+1)
    | '+' :: tl        -> [Token.{value = "+"; ttype = Binop TokenType.Plus; r; c}] @ lex_file tl r (c+1)
    | '-' :: tl        -> [Token.{value = "-"; ttype = Binop TokenType.Minus; r; c}] @ lex_file tl r (c+1)
    | '*' :: tl        -> [Token.{value = "*"; ttype = Binop TokenType.Asterisk; r; c}] @ lex_file tl r (c+1)
    | '/' :: tl        -> [Token.{value = "/"; ttype = Binop TokenType.ForwardSlash; r; c}] @ lex_file tl r (c+1)
    | '0'..'9' :: tl   -> let intlit, rest = consume_while tl (fun c -> isnum c) in
                          [Token.{value = intlit; ttype = IntegerLiteral; r; c = c+(String.length intlit)}]
                          @ lex_file rest r (c+String.length intlit)
    | hd :: tl         -> let word, rest = consume_while tl (fun c -> c = '_' || isalnum c) in
                          (match is_keyword word with
                           | Some k -> [Token.{value = word; ttype = k; r; c}] @ lex_file rest r (c+String.length word)
                           | None -> [Token.{value = word; ttype = Identifier; r; c}] @ lex_file rest r (c+String.length word))
;;

  let rec print_tokens tokens =
    match tokens with
    | [] -> ()
    | hd :: tl ->
       print_endline (Token.to_string hd);
       print_tokens tl
  ;;
end
