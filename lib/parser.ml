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
open Ast

let expect (tokens : Token.t list) (ty : TokenType.t) : Token.t * (Token.t list) =
  match tokens with
  | [] ->
     let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
     exit 1
  | hd :: tl when hd.Token.ttype = ty -> hd, tl
  | hd :: _ ->
     let msg = Printf.sprintf "Expected token of type `%s` but got `%s`" (TokenType.to_string ty) (TokenType.to_string hd.ttype) in
     let _ = Err.err Err.Expect __FILE__ __FUNCTION__ ~msg:msg (Some hd) in
     exit 1

let maybe_expect tokens ty =
  match tokens with
  | [] ->
     let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
     exit 1
  | hd :: tl when hd.Token.ttype = ty -> Some hd, tl
  | tokens -> None, tokens

let is_mut tokens =
  let open Token in
  let open TokenType in
  let rec aux = function
    | [] -> false
    | {ttype = Eof; _} :: _ -> false
    | {ttype = Semicolon; _} :: _ -> false
    | {ttype = Equals; _} :: _ -> true
    | _ :: tl -> aux tl in
  aux tokens

let rec expect_idtype (tokens : Token.t list) : TokenType.id_type * (Token.t list) =
  match tokens with
  | [] ->
     let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
     exit 1
  | {ttype = _ as ty; _} as hd :: tl ->
     (match ty with
     | TokenType.Type k -> k, tl
     | _ ->
       let msg = Printf.sprintf "Expected token of type `Type` but got `%s`" (TokenType.to_string hd.ttype) in
       let _ = Err.err Err.Expect __FILE__ __FUNCTION__ ~msg:msg (Some hd) in
       exit 1)

and parse_comma_sep_exprs tokens =
  let open Token in
  let open TokenType in

  let rec aux tokens acc =
    match tokens with
    | {ttype = RParen; _} :: tl -> acc, tl
    | _ ->
       let expr, tokens = parse_expr tokens in
       let acc = acc @ [expr] in
       (match tokens with
        | {ttype = Comma; _} :: tl -> aux tl acc
        | {ttype = RParen; _} :: tl -> acc, tl
        | _ -> acc, tokens) in

  aux tokens []

and parse_primary_expr tokens =
  let open Token in

  let rec aux tokens left =
    match tokens with
    | [] ->
       let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
       exit 1
    | {ttype = Identifier; _} as id :: tl ->
       (match left with
        | Some (Ast.Term _) ->
           let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"illegal primary expression" @@ Some id in
           exit 1
        | _ -> ());
       aux tl @@ Some (Ast.Term (Ast.Ident id))
    | {ttype = IntegerLiteral; _} as intlit :: tl -> aux tl @@ Some (Ast.Term (Ast.IntLit intlit))
    | {ttype = StringLiteral; _} as strlit :: tl -> aux tl @@ Some (Ast.Term (Ast.StrLit strlit))
    | {ttype = LParen; _} :: tl ->
       let args, tokens = parse_comma_sep_exprs tl in
       (match left with
        (* Procedure call *)
        | Some lhs -> aux tokens @@ Some (Ast.Term (Ast.Proc_Call Ast.{lhs; args}))
        (* Tuple *)
        | _ when List.length args > 1 -> failwith "tuples are unimplemented"
        (* Math *)
        | _ -> aux tokens @@ Some (List.hd args))
    | _ -> left, tokens in

  aux tokens None

and parse_multiplicative_expr tokens =
  let open Token in
  let open TokenType in

  let rec aux tokens lhs =
    match tokens with
    | {ttype = Asterisk; _} | {ttype = ForwardSlash; _} | {ttype = Percent; _} as op :: tl ->
       let rhs, tokens = match parse_primary_expr tl with
         | Some expr, tokens -> expr, tokens
         | _ -> failwith "invalid expression" in
       aux tokens (Ast.Binary {lhs; rhs; op})
    | _ -> lhs, tokens in

  let lhs, tokens = match parse_primary_expr tokens with
    | Some expr, tokens -> expr, tokens
    | _ -> failwith "invalid expression" in
  aux tokens lhs

and parse_additive_expr tokens =
  let open Token in
  let open TokenType in

  let rec aux tokens lhs =
    match tokens with
    | {ttype = Plus; _} | {ttype = Minus; _} as op :: tl ->
       let rhs, tokens = parse_multiplicative_expr tl in
       aux tokens (Ast.Binary {lhs; rhs; op})
    | _ -> lhs, tokens in

  let lhs, tokens = parse_multiplicative_expr tokens in
  aux tokens lhs

and parse_equalitative_expr tokens =
  let open Token in
  let open TokenType in

  let rec aux tokens lhs =
    match tokens with
    | {ttype = DoubleEquals; _} | {ttype = GreaterThan; _}
      | {ttype = GreaterThanEqual; _} | {ttype = LessThanEqual; _}
      | {ttype = NotEqual; _} | {ttype = LessThan; _} as op :: tl ->
       let rhs, tokens = parse_additive_expr tl in
       aux tokens (Ast.Binary {lhs; rhs; op})
    | _ -> lhs, tokens in

  let lhs, tokens = parse_additive_expr tokens in
  aux tokens lhs

and parse_logical_expr tokens =
  let open Token in
  let open TokenType in

  let rec aux tokens lhs =
    match tokens with
    | {ttype = DoubleAmpersand; _} | {ttype = DoublePipe; _} as op :: tl ->
       let rhs, tokens = parse_equalitative_expr tl in
       aux tokens (Ast.Binary {lhs; rhs; op})
    | _ -> lhs, tokens in

  let lhs, tokens = parse_equalitative_expr tokens in
  aux tokens lhs

and parse_expr tokens =
  let expr, tokens = parse_logical_expr tokens in
  expr, tokens

and parse_stmt_expr tokens =
  let expr, tokens = parse_expr tokens in
  let _, tokens = expect tokens Semicolon in
  expr, tokens

and parse_stmt_return tokens =
  let expr, tokens = parse_expr tokens in
  let _, tokens = expect tokens Semicolon in
  expr, tokens

and parse_stmt_for tokens =
  let open TokenType in
  let start, tokens = parse_stmt tokens in
  let _while, tokens = parse_expr tokens in
  let _, tokens = expect tokens Semicolon in
  let _end, tokens = parse_stmt tokens in
  let block, tokens = parse_stmt_block tokens in
  Ast.{start; _while; _end; block}, tokens

and parse_mut_stmt tokens =
  let left, tokens = parse_expr tokens in
  let op, tokens = expect tokens Equals in
  let right, tokens = parse_expr tokens in
  let _, tokens = expect tokens Semicolon in
  Ast.{left; op; right}, tokens

and parse_stmt tokens =
  let open Token in
  let open TokenType in

  match tokens with
  | {ttype = Export; _} :: {ttype = Proc; _} :: tl ->
     let stmt, tokens = parse_stmt_proc tl true in
     Proc stmt, tokens
  | {ttype = Proc; _} :: tl ->
     let stmt, tokens = parse_stmt_proc tl false in
     Proc stmt, tokens
  | {ttype = Let; _} :: tl ->
     let stmt, tokens = parse_stmt_let tl in
     Let stmt, tokens
  | ({ttype = Identifier; _} :: tl) as tokens when is_mut tokens ->
     let stmt, tokens = parse_mut_stmt tokens in
     Mut stmt, tokens
  | ({ttype = Identifier; _} :: _) as tokens ->
     let stmt, tokens = parse_stmt_expr tokens in
     Stmt_Expr stmt, tokens
  | {ttype = Return; _} :: tl ->
     let stmt, tokens = parse_stmt_return tl in
     Return stmt, tokens
  | {ttype = For; _} :: tl ->
     let stmt, tokens = parse_stmt_for tl in
     For stmt, tokens
  | {ttype = If; _} :: tl -> failwith "if todo"
  | [] ->
     let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"no more tokens" @@ None in
     exit 1
  | hd :: _ ->
     let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"invalid stmt" @@ Some hd in
     exit 1

and parse_stmt_import tokens =
  let filepath, tokens = expect tokens TokenType.StringLiteral in
  let _, tokens = expect tokens TokenType.Semicolon in
  Ast.{filepath}, tokens

and parse_stmt_block (tokens : Token.t list) : (Ast.stmt list) * Token.t list =
  let open Token in
  let open TokenType in

  let rec aux tokens acc =
    match tokens with
    | [] -> acc, []
    | {ttype = RBrace; _} :: tl -> acc, tl
    | tokens ->
       let stmt, tokens = parse_stmt tokens in
       aux tokens (acc @ [stmt]) in

  let _, tokens = expect tokens LBrace in
  aux tokens []

and parse_stmt_let tokens =
  let open TokenType in

  let id, tokens = expect tokens Identifier in
  let _, tokens = expect tokens Colon in
  let ty, tokens = expect_idtype tokens in
  let _, tokens = expect tokens Equals in
  let expr, tokens = parse_expr tokens in
  let _, tokens = expect tokens Semicolon in

  Ast.{id; ty; expr; export=false}, tokens

and parse_stmt_proc tokens export =
  let open Token in
  let open TokenType in

  let rec parse_parameters' tokens acc =
    match tokens with
    | [] -> acc, []
    | {ttype = RParen; _} :: _ -> acc, tokens
    | tokens ->
       let id, tokens = expect tokens Identifier in
       let _, tokens = expect tokens Colon in
       let ty, tokens = expect_idtype tokens in
       let acc = acc @ [id, ty] in
       (match tokens with
        | {ttype = Comma; _} :: tl -> parse_parameters' tl acc
        | _ -> acc, tokens) in

  let parse_parameters = function
    | {ttype = (Type Void); _} :: tl -> [], tl
    | tokens -> parse_parameters' tokens [] in

  let _, tokens = maybe_expect tokens Export in
  let id, tokens = expect tokens Identifier in
  let _, tokens = expect tokens LParen in
  let params, tokens = parse_parameters tokens in
  let _, tokens = expect tokens RParen in
  let _, tokens = expect tokens Colon in
  let rettype, tokens = expect_idtype tokens in
  let block, tokens = parse_stmt_block tokens in
  Ast.{id; params; rettype; block; export}, tokens

let parse_stmt_module tokens =
  let id, tokens = expect tokens TokenType.Identifier in
  let _, tokens = expect tokens TokenType.Where in
  Ast.{id}, tokens

(* Parses the top-most statements (proc defs, global vars etc). *)
let parse_toplvl_stmt tokens =
  let open Token in
  let open TokenType in

  match tokens with
  | {ttype = Export; _} :: {ttype = Proc; _} :: tl ->
     let stmt, tokens = parse_stmt_proc tl true in
     Ast.Proc_Def stmt, tokens
  | {ttype = Proc; _} :: tl ->
     let stmt, tokens = parse_stmt_proc tl false in
     Ast.Proc_Def stmt, tokens
  | {ttype = Let; _} :: tl ->
     let stmt, tokens = parse_stmt_let tl in
     Ast.Let stmt, tokens
  | {ttype = Import; _} :: tl ->
     let stmt, tokens = parse_stmt_import tl in
     Ast.Import stmt, tokens
  | {ttype = Module; _} :: tl ->
     let stmt, tokens = parse_stmt_module tl in
     Ast.Module stmt, tokens
  | hd :: _ ->
     let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"invalid top level stmt" @@ Some hd in
     exit 1
  | _ ->
     let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
     exit 1

(* Entrypoint of the parser. Takes a list of tokens and produces
 * a program. *)
let produce_ast (tokens : Token.t list) : Ast.program =
  ignore Ast.debug_print_expr;

  let rec aux = function
    | [] -> []
    | hd :: _ when hd.Token.ttype = TokenType.Eof -> []
    | tokens' ->
       let stmt, rest = parse_toplvl_stmt tokens' in
       [stmt] @ aux rest in

  aux tokens

