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

let err msg =
  let _ = Printf.printf "[Lexer ERR]: %s\n" msg in
  exit 1

let is_symbol c =
  match Hashtbl.find_opt symbols c with
  | Some t -> Some t
  | None -> None

let is_keyword s =
  match Hashtbl.find_opt keywords s with
  | Some t -> Some t
  | None -> None

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

let peek lst ahead =
  let rec peek' lst i =
    match lst with
    | [] -> None
    | hd :: _ when i = ahead -> Some hd
    | _ :: tl -> peek' tl (i+1)
  in
  peek' lst 0
;;

let lex_file src =
  let rec lex_file' r c lst =
    match lst with
    | [] -> [Token.{value = "Eof"; ttype = Eof}]
    | hd :: tl when hd = '\n' -> lex_file' (r+1) 1 tl
    | hd :: tl when isignorable hd -> lex_file' r (c+1) tl
    | hd :: tl -> failwith "todo"
    | _ -> err @@ Printf.sprintf "Unexpected character at line: %d char: %d" r c
  in
  lex_file' 1 1 (src |> String.to_seq |> List.of_seq)
;;

let file_to_str filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let rec print_tokens tokens =
  match tokens with
  | [] -> ()
  | hd :: tl ->
    print_endline (Token.to_string hd);
    print_tokens tl

let filepath = "./input.txt"
let () =
  let data = file_to_str filepath in
  let tokens = lex_file data in
  print_tokens tokens
;;
