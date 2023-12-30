open Token

let symbols : (char, TokenType.t) Hashtbl.t = Hashtbl.create 15
let keywords : (string, TokenType.t) Hashtbl.t = Hashtbl.create 20

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
  let rec lex_file' r c t lst =
    match lst with
    | [] -> t @ [Token.{value = "EOF"; ttype = Eof}]
    | hd :: tl when hd = '\n' -> lex_file' (r+1) 0 t tl
    | hd :: tl when isignorable hd -> lex_file' r (c+1) t tl
    | _ -> failwith "unimplemented"
  in
  lex_file' 0 0 [] (src |> String.to_seq |> List.of_seq)
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
