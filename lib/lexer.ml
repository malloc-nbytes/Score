(* MIT License

   * Copyright (c) 2023 malloc-nbytes

   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:

   * The above copyright notice and this permission notice shall be included in all
   * copies or substantial portions of the Software.

   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   * SOFTWARE. *)

open Token

let keywords : (string, TokenType.t) Hashtbl.t = Hashtbl.create 30

(* Fill the keywords hashtable with the correct words
 * and token type. Should be called before `lex_file ()` is called. *)
let populate_keywords () : unit =
  let _ = Hashtbl.add keywords "i32"    @@ TokenType.Type TokenType.I32 in
  let _ = Hashtbl.add keywords "char"   @@ TokenType.Type TokenType.Char in
  let _ = Hashtbl.add keywords "usize"  @@ TokenType.Type TokenType.Usize in
  let _ = Hashtbl.add keywords "str"    @@ TokenType.Type TokenType.Str in
  let _ = Hashtbl.add keywords "void"   @@ TokenType.Type TokenType.Void in
  let _ = Hashtbl.add keywords "proc"      TokenType.Proc in
  let _ = Hashtbl.add keywords "return"    TokenType.Return in
  let _ = Hashtbl.add keywords "let"       TokenType.Let in
  let _ = Hashtbl.add keywords "if"        TokenType.If in
  let _ = Hashtbl.add keywords "else"      TokenType.Else in
  let _ = Hashtbl.add keywords "while"     TokenType.While in
  let _ = Hashtbl.add keywords "break"     TokenType.Break in
  let _ = Hashtbl.add keywords "for"       TokenType.For in
  let _ = Hashtbl.add keywords "struct"    TokenType.Struct in
  let _ = Hashtbl.add keywords "ref"       TokenType.Ref in
  let _ = Hashtbl.add keywords "import"    TokenType.Import in
  let _ = Hashtbl.add keywords "export"    TokenType.Export in
  let _ = Hashtbl.add keywords "def"       TokenType.Def in
  let _ = Hashtbl.add keywords "macro"     TokenType.Macro in
  let _ = Hashtbl.add keywords "in"        TokenType.In in
  let _ = Hashtbl.add keywords "null"      TokenType.Null in
  let _ = Hashtbl.add keywords "module"    TokenType.Module in
  let _ = Hashtbl.add keywords "where"     TokenType.Where in
  let _ = Hashtbl.add keywords "true"      TokenType.True in
  let _ = Hashtbl.add keywords "false"     TokenType.False in
  let _ = Hashtbl.add keywords "extern"    TokenType.Extern in
  ()

let rec repl_quote (lst : char list) : string =
  match lst with
  | [] -> ""
  | hd :: tl -> (if hd = '\'' then "_QUOTE_" else String.make 1 hd) ^ repl_quote tl

(* Determines if the given string `s` is a
 * keyword or not. If it is, it returns the
 * appropriate token type. *)
let is_keyword (s : string) : TokenType.t option =
  match Hashtbl.find_opt keywords s with
  | Some t -> Some t
  | None -> None

(* Takes a character and determines if
 * it is an alpha character. *)
let isalpha (c : char) : bool =
  let c = int_of_char c in
  (c >= 65 && c <= 90) || (c >= 97 && c <= 122)

(* Takes a character and determines if
 * it is a number character. *)
let isnum (c : char) : bool =
  let c = int_of_char c in
  let c = c - int_of_char '0' in
  (c >= 0) && (c <= 9)

(* Takes a character and determines if
 * it is alphanumeric. *)
let isalnum (c : char) : bool = isalpha c || isnum c

(* Takes a list of characters and a predicate. It will accumulate
 * chars while the char satisfies `predicate`. It will then return
 * the accumulated chars as a string, as well as the rest. *)
let consume_while (lst : char list) (predicate : char -> bool) : string * char list =
  let rec aux lst acc =
    match lst with
    | [] -> acc, []
    | hd :: tl when predicate hd -> aux tl (acc ^ String.make 1 hd)
    | hd :: tl -> acc, hd :: tl
  in
  aux lst ""

(* Given `src` (source code converted to a char list), will lex
 * all chars into tokens. `r` and `c` are the rows and columns that
 * will be added to a created token for error reporting. *)
let rec lex_file (fp : string) (src : char list) (r : int) (c : int) : Token.t list =
  match src with

  (* We are done with lexing *)
  | [] -> [Token.{lexeme = "Eof"; ttype = TokenType.Eof; r; c; fp}]

  (* Ignorable chars *)
  | '\n' :: tl -> lex_file fp tl (r+1) 1
  | '\t' :: tl -> lex_file fp tl r (c+1)
  | ' ' :: tl -> lex_file fp tl r (c+1)

  (* Comments *)
  | '-' :: '-' :: tl ->
     let comment, rest = consume_while tl (fun c -> c <> '\n') in
     lex_file fp rest r (c+2+String.length comment)

  (* Multi-char symbols *)
  | '\'' :: chara :: '\'' :: tl -> [Token.{lexeme = String.make 1 chara; ttype = Character; r; c; fp}] @ lex_file fp tl r (c+3)
  | ':' :: ':' :: tl -> [Token.{lexeme = "::"; ttype = DoubleColon; r; c; fp}]          @ lex_file fp tl r (c+2)
  | '-' :: '>' :: tl -> [Token.{lexeme = "->"; ttype = RightArrow; r; c; fp}]           @ lex_file fp tl r (c+2)
  | '=' :: '=' :: tl -> [Token.{lexeme = "=="; ttype = DoubleEquals; r; c; fp}]         @ lex_file fp tl r (c+2)
  | '&' :: '&' :: tl -> [Token.{lexeme = "&&"; ttype = DoubleAmpersand; r; c; fp}]      @ lex_file fp tl r (c+2)
  | '|' :: '|' :: tl -> [Token.{lexeme = "||"; ttype = DoublePipe; r; c; fp}]           @ lex_file fp tl r (c+2)
  | '<' :: '=' :: tl -> [Token.{lexeme = "<="; ttype = LessThanEqual; r; c; fp}]        @ lex_file fp tl r (c+2)
  | '>' :: '=' :: tl -> [Token.{lexeme = ">="; ttype = GreaterThanEqual; r; c; fp}]     @ lex_file fp tl r (c+2)
  | '!' :: '=' :: tl -> [Token.{lexeme = "!="; ttype = NotEqual; r; c; fp}]             @ lex_file fp tl r (c+2)
  | '+' :: '=' :: tl -> [Token.{lexeme = "+="; ttype = PlusEquals; r; c; fp}]           @ lex_file fp tl r (c+2)
  | '-' :: '=' :: tl -> [Token.{lexeme = "-="; ttype = MinusEquals; r; c; fp}]          @ lex_file fp tl r (c+2)
  | '*' :: '=' :: tl -> [Token.{lexeme = "*="; ttype = AsteriskEquals; r; c; fp}]       @ lex_file fp tl r (c+2)
  | '/' :: '=' :: tl -> [Token.{lexeme = "/="; ttype = ForwardSlashEquals; r; c; fp}]   @ lex_file fp tl r (c+2)
  | '%' :: '=' :: tl -> [Token.{lexeme = "%="; ttype = PercentEquals; r; c; fp}]        @ lex_file fp tl r (c+2)
  | '.' :: '.' :: '.' :: tl -> [Token.{lexeme = "..."; ttype = TriplePeriod; r; c; fp}] @ lex_file fp tl r (c+2)

  (* String literals *)
  | '"' :: tl ->
     let strlit, rest = consume_while tl (fun c -> c <> '"') in
     if rest = []
     then
       let _ = Err.err Err.Fatal
                 __FILE__ __FUNCTION__
                 ~msg:(Printf.sprintf "unterminated string literal: %s" strlit)
                 None in exit 1
     else
       let rest = List.tl rest in (* consume_while does not consume closing quote. *)
       [Token.{lexeme = strlit; ttype = StringLiteral; r; c = c+2+(String.length strlit); fp}]
       @ lex_file fp rest r (c+2+String.length strlit)

  (* Single-char symbols *)
  | '>' :: tl -> [Token.{lexeme = ">"; ttype = GreaterThan; r; c; fp}]      @ lex_file fp tl r (c+1)
  | '<' :: tl -> [Token.{lexeme = "<"; ttype = LessThan; r; c; fp}]         @ lex_file fp tl r (c+1)
  | ':' :: tl -> [Token.{lexeme = ":"; ttype = Colon; r; c; fp}]            @ lex_file fp tl r (c+1)
  | ',' :: tl -> [Token.{lexeme = ","; ttype = Comma; r; c; fp}]            @ lex_file fp tl r (c+1)
  | '(' :: tl -> [Token.{lexeme = "("; ttype = LParen; r; c; fp}]           @ lex_file fp tl r (c+1)
  | ')' :: tl -> [Token.{lexeme = ")"; ttype = RParen; r; c; fp}]           @ lex_file fp tl r (c+1)
  | '{' :: tl -> [Token.{lexeme = "{"; ttype = LBrace; r; c; fp}]           @ lex_file fp tl r (c+1)
  | '}' :: tl -> [Token.{lexeme = "}"; ttype = RBrace; r; c; fp}]           @ lex_file fp tl r (c+1)
  | '[' :: tl -> [Token.{lexeme = "["; ttype = LBracket; r; c; fp}]         @ lex_file fp tl r (c+1)
  | ']' :: tl -> [Token.{lexeme = "]"; ttype = RBracket; r; c; fp}]         @ lex_file fp tl r (c+1)
  | ';' :: tl -> [Token.{lexeme = ";"; ttype = Semicolon; r; c; fp}]        @ lex_file fp tl r (c+1)
  | '+' :: tl -> [Token.{lexeme = "+"; ttype = Plus; r; c; fp}]             @ lex_file fp tl r (c+1)
  | '-' :: tl -> [Token.{lexeme = "-"; ttype = Minus; r; c; fp}]            @ lex_file fp tl r (c+1)
  | '*' :: tl -> [Token.{lexeme = "*"; ttype = Asterisk; r; c; fp}]         @ lex_file fp tl r (c+1)
  | '/' :: tl -> [Token.{lexeme = "/"; ttype = ForwardSlash; r; c; fp}]     @ lex_file fp tl r (c+1)
  | '%' :: tl -> [Token.{lexeme = "%"; ttype = Percent; r; c; fp}]          @ lex_file fp tl r (c+1)
  | '=' :: tl -> [Token.{lexeme = "="; ttype = TokenType.Equals; r; c; fp}] @ lex_file fp tl r (c+1)
  | '.' :: tl -> [Token.{lexeme = "."; ttype = TokenType.Period; r; c; fp}] @ lex_file fp tl r (c+1)
  | '&' :: tl -> [Token.{lexeme = "&"; ttype = TokenType.Ampersand; r; c; fp}] @ lex_file fp tl r (c+1)

  (* Integer literals *)
  | '0'..'9' as hd :: tl ->
     let intlit, rest = consume_while (hd :: tl) (fun c -> isnum c) in
     [Token.{lexeme = intlit;
             ttype = IntegerLiteral;
             r; c = c+(String.length intlit); fp}]
     @ lex_file fp rest r (c+String.length intlit)

  (* Identifier or keyword *)
  | hd :: tl ->
     let word, rest = consume_while tl (fun c -> c = '_' || c = '\''|| isalnum c) in
     let word = String.make 1 hd ^ word in
     let word_wo_quotes = repl_quote (word |> String.to_seq |> List.of_seq) in
     (match is_keyword word with

      (* Keyword *)
      | Some k ->
         [Token.{lexeme = word; ttype = k; r; c; fp}] @ lex_file fp rest r (c+String.length word)

      (* Identifier *)
      | None ->
         [Token.{lexeme = word_wo_quotes; ttype = Identifier; r; c; fp}] @ lex_file fp rest r (c+String.length word))

(* Debug function to print a list of tokens. *)
let rec print_tokens (tokens : Token.t list) : unit =
  match tokens with
  | [] -> ()
  | hd :: tl ->
     print_endline (Token.to_string hd);
     print_tokens tl
