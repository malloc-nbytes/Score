module Lexer = struct
  open Token

  let keywords : (string, TokenType.t) Hashtbl.t = Hashtbl.create 20

  (* Fill the keywords hashtable with the correct words
   * and token type. Should be called before `lex_file ()` is called. *)
  let populate_keywords () : unit =
    let _ = Hashtbl.add keywords "proc" @@ Keyword Proc in
    let _ = Hashtbl.add keywords "ret" @@ Keyword Ret in
    let _ = Hashtbl.add keywords "let" @@ Keyword Let in
    let _ = Hashtbl.add keywords "i32" Type in
    let _ = Hashtbl.add keywords "void" Type in
    ()
  ;;

  (* Takes a message and prints it for error logging.
   * Exits the program. *)
  let err (msg : string) : unit =
    let _ = Printf.printf "[Lexer ERR]: %s\n" msg in
    exit 1
  ;;

  (* Determines if the given string `s` is a
   * keyword or not. If it is, it returns the
   * appropriate token type. *)
  let is_keyword (s : string) : TokenType.t option =
    match Hashtbl.find_opt keywords s with
    | Some t -> Some t
    | None -> None
  ;;

  (* Takes a character and determines if
   * it is an alpha character. *)
  let isalpha (c : char) : bool =
    let c = int_of_char c in
    (c >= 65 && c <= 90) || (c >= 97 && c <= 122)
  ;;

  (* Takes a character and determines if
   * it is a number character. *)
  let isnum (c : char) : bool =
    let c = int_of_char c in
    let c = c - int_of_char '0' in
    (c >= 0) && (c <= 9)
  ;;

  (* Takes a character and determines if
   * it is alphanumeric. *)
  let isalnum (c : char) : bool = isalpha c || isnum c;;

  (* Takes a list of characters and a predicate. It will accumulate
   * chars until a char satisfies `predicate`. It will then return
   * the accumulated chars as a string, as well as the rest *)
  let consume_while (lst : char list) (predicate : char -> bool) : string * char list =
    let rec aux lst acc =
      match lst with
      | [] -> acc, []
      | hd :: tl when predicate hd -> aux tl (acc ^ String.make 1 hd)
      | hd :: tl -> acc, hd :: tl
    in
    aux lst ""
  ;;

  (* Given `src` (source code converted to a char list), will lex
   * all chars into tokensl `r` and `c` are the rows and columns that
   * will be added to a created token. *)
  let rec lex_file (src : char list) (r : int) (c : int) : Token.t list =
    match src with
    | []               -> [Token.{value = "Eof"; ttype = TokenType.Eof; r; c}]
    | '\n' :: tl       -> lex_file tl (r+1) 1
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
    | ':' :: tl        -> [Token.{value = ":"; ttype = Colon; r; c}] @ lex_file tl r (c+1)
    | ',' :: tl        -> [Token.{value = ","; ttype = Comma; r; c}] @ lex_file tl r (c+1)
    | '(' :: tl        -> [Token.{value = "("; ttype = LParen; r; c}] @ lex_file tl r (c+1)
    | ')' :: tl        -> [Token.{value = ")"; ttype = RParen; r; c}] @ lex_file tl r (c+1)
    | '{' :: tl        -> [Token.{value = "{"; ttype = LBrace; r; c}] @ lex_file tl r (c+1)
    | '}' :: tl        -> [Token.{value = "}"; ttype = RBrace; r; c}] @ lex_file tl r (c+1)
    | '[' :: tl        -> [Token.{value = "["; ttype = LBracket; r; c}] @ lex_file tl r (c+1)
    | ']' :: tl        -> [Token.{value = "]"; ttype = RBracket; r; c}] @ lex_file tl r (c+1)
    | ';' :: tl        -> [Token.{value = ";"; ttype = Semicolon; r; c}] @ lex_file tl r (c+1)
    | '+' :: tl        -> [Token.{value = "+"; ttype = Binop; r; c}] @ lex_file tl r (c+1)
    | '-' :: tl        -> [Token.{value = "-"; ttype = Binop; r; c}] @ lex_file tl r (c+1)
    | '*' :: tl        -> [Token.{value = "*"; ttype = Binop; r; c}] @ lex_file tl r (c+1)
    | '/' :: tl        -> [Token.{value = "/"; ttype = Binop; r; c}] @ lex_file tl r (c+1)
    | '%' :: tl        -> [Token.{value = "%"; ttype = Binop; r; c}] @ lex_file tl r (c+1)
    | '=' :: tl        -> [Token.{value = "="; ttype = TokenType.Equals; r; c}] @ lex_file tl r (c+1)
    | '0'..'9' :: tl   -> let intlit, rest = consume_while tl (fun c -> isnum c) in
                          [Token.{value = intlit; ttype = IntegerLiteral; r; c = c+(String.length intlit)}]
                          @ lex_file rest r (c+String.length intlit)
    | hd :: tl         -> let word, rest = consume_while tl (fun c -> c = '_' || isalnum c) in
                          let word = String.make 1 hd ^ word in
                          (match is_keyword word with
                           | Some k -> [Token.{value = word; ttype = k; r; c}] @ lex_file rest r (c+String.length word)
                           | None -> [Token.{value = word; ttype = Identifier; r; c}] @ lex_file rest r (c+String.length word))
  ;;

  (* Debug function to print a list of tokens. *)
  let rec print_tokens (tokens : Token.t list) : unit =
    match tokens with
    | [] -> ()
    | hd :: tl ->
       print_endline (Token.to_string hd);
       print_tokens tl
  ;;

end
