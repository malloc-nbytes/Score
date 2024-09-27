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

let rec expect (tokens : Token.t list) (ty : TokenType.t) : Token.t * (Token.t list) =
  match tokens with
  | [] ->
     let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
     exit 1
  | hd :: tl when hd.Token.ttype = ty -> hd, tl
  | hd :: _ ->
     let msg = Printf.sprintf "Expected token of type `%s` but got `%s`" (TokenType.to_string ty) (TokenType.to_string hd.ttype) in
     let _ = Err.err Err.Expect __FILE__ __FUNCTION__ ~msg:msg (Some hd) in
     exit 1

and maybe_expect tokens ty =
  match tokens with
  | [] -> None, []
  | hd :: tl when hd.Token.ttype = ty -> Some hd, tl
  | tokens -> None, tokens

let rec parse_parameters tokens =
  let open Token in
  let rec parse_parameters' tokens acc =
    match tokens with
    | [] -> acc, [], false
    | {ttype = RParen; _} :: _ -> acc, tokens, false
    | {ttype = TriplePeriod; _} :: tl ->
       acc, tl, true
    | tokens ->
       let id, tokens = expect tokens Identifier in
       let _, tokens = expect tokens Colon in
       let ty, tokens = expect_idtype tokens None in
       let acc = acc @ [id, ty] in
       (match tokens with
        | {ttype = Comma; _} :: tl -> parse_parameters' tl acc
        | _ -> acc, tokens, false) in
  match tokens with
  | {ttype = (Type Void); _} :: tl -> [], tl, false
  | tokens -> parse_parameters' tokens []

and is_mut_stmt tokens =
  let open Token in
  let open TokenType in
  let rec aux = function
    | [] -> false
    | {ttype = Eof; _} :: _ -> false
    | {ttype = Semicolon; _} :: _ -> false
    | {ttype = Equals; _} :: _ -> true
    | _ :: tl -> aux tl in
  aux tokens

and unwrap : 'a = function
  | Some k -> k
  | None ->
     let _ = Err.err Err.Expect __FILE__ __FUNCTION__ ~msg:"tried to unwrap a None value" None in
     exit 1

and expect_idtype (tokens : Token.t list) (prev : TokenType.id_type option) : TokenType.id_type * (Token.t list) =
  let open TokenType in
  match tokens with
  | [] ->
     let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
     exit 1
  | {ttype = Asterisk; _} :: tl when prev <> None -> expect_idtype tl (Some (Pointer (unwrap prev)))
  | {ttype = LBracket; _} :: tl when prev <> None ->
     let len, tokens = maybe_expect tl IntegerLiteral in
     let len = match len with
       | Some i -> Some (int_of_string i.lexeme)
       | None -> None in
     let _, tokens = expect tokens RBracket in
     expect_idtype tokens (Some (Array (unwrap prev, len)))
  | {ttype = Type k; _} :: tl -> expect_idtype tl (Some k)
  | tokens when prev <> None -> unwrap prev, tokens
  | hd :: _ ->
     let msg = Printf.sprintf "Expected token of type `Type` but got `%s`" (TokenType.to_string hd.ttype) in
     let _ = Err.err Err.Expect __FILE__ __FUNCTION__ ~msg:msg (Some hd) in
     exit 1

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

(* Parses a group of comma separated "members" in the form of id: <type>. *)
and parse_ids_and_types_group (tokens : Token.t list) : ((Token.t * TokenType.id_type) list) * (Token.t list) =
  let open Token in
  let open TokenType in

  let rec aux tokens acc =
    match tokens with
    | [] -> acc, tokens
    | {ttype = RBrace; _} :: tl -> acc, tl
    | tokens ->
      let id, tokens = expect tokens Identifier in
      let _, tokens = expect tokens Colon in
      let ty, tokens = expect_idtype tokens None in
      let acc = acc @ [id, ty] in
      (match tokens with
       | {ttype = Comma; _} :: tl -> aux tl acc
       | _ ->
          let _, tokens = expect tokens RBrace in
          acc, tokens) in

  let _, tokens = expect tokens LBrace in
  aux tokens []

and parse_primary_expr tokens =
  let open Token in

  let rec aux tokens (left : Ast.expr option) =
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
    | {ttype; lexeme = value; _} :: tl when ttype = True || ttype = False -> aux tl @@ Some (Ast.Term (Ast.BoolLit (bool_of_string value)))
    | {ttype = DoubleColon; _} :: tl ->
       let left = match left with
         | Some (Ast.Term (Ast.Ident t)) -> t
         | _ -> failwith "cannot use namespace operator `::` without left identifier"
       and right, tokens = match parse_expr tl with
         | Ast.Term t, tokens -> t, tokens
         | _ -> failwith "cannot use namespace operator `::` without right term expression" in
       aux tokens @@ Some (Ast.Term (Ast.Namespace Ast.{left; right}))
    | ({ttype = Type _; _} :: tl) as tokens ->
       let _type, tokens = expect_idtype tokens None in
       let rhs, tokens = parse_expr tokens in
       aux tokens @@ Some (Ast.Term (Ast.Cast {_type; rhs}))
    | {ttype = LParen; _} :: tl ->
       let args, tokens = parse_comma_sep_exprs tl in
       (match left with
        (* Procedure call *)
        | Some (Ast.Term (Ast.Ident id)) -> aux tokens @@ Some (Ast.Term (Ast.Proc_Call Ast.{lhs=id; args}))
        (* | Some lhs -> aux tokens @@ Some (Ast.Term (Ast.Proc_Call Ast.{lhs; args})) *)
        (* Tuple *)
        | _ when List.length args > 1 -> failwith "tuples are unimplemented"
        (* Math *)
        | _ -> aux tokens @@ Some (List.hd args))
    | {ttype = LBracket; _} :: tl ->
       let accessor = match left with
         | Some l -> l
         | None -> failwith "array indexing must have a left expression" in
       let idx, tokens = parse_expr tl in
       let _, tokens = expect tokens RBracket in
       aux tokens @@ Some (Ast.Term (Index {accessor; idx}))
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

and parse_stmt_mut tokens =
  let left, tokens = parse_expr tokens in
  let op, tokens = expect tokens Equals in
  let right, tokens = parse_expr tokens in
  let _, tokens = expect tokens Semicolon in
  Ast.{left; op; right}, tokens

and parse_stmt_if tokens =
  let open Token in
  let open TokenType in

  let expr, tokens = parse_expr tokens in
  let block, tokens = parse_stmt_block tokens in
  match tokens with
  | {ttype = Else; _} :: {ttype = If; _} :: tl ->
     let if_, tokens = parse_stmt_if tl in
     let else_ = [Ast.If if_] in
     Ast.{expr; block; _else = Some else_}, tokens
  | {ttype = TokenType.Else; _} :: tl ->
     let _else, tokens = parse_stmt_block tl in
     Ast.{expr; block; _else = Some _else}, tokens
  | _ -> Ast.{expr; block; _else = None}, tokens

and parse_stmt_while tokens =
  let expr, tokens = parse_expr tokens in
  let block, tokens = parse_stmt_block tokens in
  Ast.{expr; block}, tokens

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
  | ({ttype = Identifier; _} :: _) as tokens when is_mut_stmt tokens ->
     let stmt, tokens = parse_stmt_mut tokens in
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
  | {ttype = While; _} :: tl ->
     let stmt, tokens = parse_stmt_while tl in
     While stmt, tokens
  | {ttype = If; _} :: tl ->
     let stmt, tokens = parse_stmt_if tl in
     If stmt, tokens
  | [] ->
     let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"no more tokens" @@ None in
     exit 1
  | hd :: _ ->
     let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"invalid stmt" @@ Some hd in
     exit 1

(* Parses an `import` statement *)
and parse_stmt_import tokens =
  let filepath, tokens = expect tokens TokenType.StringLiteral in
  let _, tokens = expect tokens TokenType.Semicolon in
  Ast.{filepath}, tokens

(* Parses a `block` statement (which is a list of statements
 * that is surrounded by { }.*)
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

(* Parses a `let` statement. *)
and parse_stmt_let tokens =
  let open TokenType in

  let id, tokens = expect tokens Identifier in
  let _, tokens = expect tokens Colon in
  let ty, tokens = expect_idtype tokens None in
  let _, tokens = expect tokens Equals in
  let expr, tokens = parse_expr tokens in
  let _, tokens = expect tokens Semicolon in

  Ast.{id; ty; expr; export=false}, tokens

and parse_stmt_proc tokens export =
  let open Token in
  let open TokenType in

  let id, tokens = expect tokens Identifier in
  let _, tokens = expect tokens LParen in
  let params, tokens, variadic = parse_parameters tokens in
  let _, tokens = expect tokens RParen in
  let _, tokens = expect tokens Colon in
  let rettype, tokens = expect_idtype tokens None in
  let block, tokens = parse_stmt_block tokens in
  Ast.{id; params; rettype; block; export; variadic}, tokens

and parse_stmt_def tokens export : Ast.stmt_def * Token.t list =
  let open TokenType in
  let id, tokens = expect tokens Identifier in
  let _, tokens = expect tokens LParen in
  let params, tokens, variadic = parse_parameters tokens in
  let _, tokens = expect tokens RParen in
  let _, tokens = expect tokens Colon in
  let rettype, tokens = expect_idtype tokens None in
  let _, tokens = expect tokens Semicolon in
  Ast.{id; params; rettype; export; variadic}, tokens

and parse_stmt_extern tokens export : Ast.stmt_extern * Token.t list =
  let open TokenType in
  let id, tokens = expect tokens Identifier in
  let _, tokens = expect tokens LParen in
  let params, tokens, variadic = parse_parameters tokens in
  let _, tokens = expect tokens RParen in
  let _, tokens = expect tokens Colon in
  let rettype, tokens = expect_idtype tokens None in
  let _, tokens = expect tokens Semicolon in
  Ast.{id; params; rettype; export; variadic}, tokens

and parse_stmt_module tokens =
  let open TokenType in
  let id, tokens = expect tokens Identifier in
  let _, tokens = expect tokens Where in
  Ast.{id}, tokens

and parse_stmt_struct tokens export =
  let open TokenType in
  let id, tokens = expect tokens Identifier in
  let members, tokens = parse_ids_and_types_group tokens in
  Ast.{id; members; export}, tokens

(* Parses the top-most statements (proc defs, structs, global vars etc). *)
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
  | {ttype = Def; _} :: tl ->
    let (stmt : Ast.stmt_def), tokens = parse_stmt_def tl false in
    Ast.Def stmt, tokens
  | {ttype = Extern; _} :: tl ->
     let (stmt : Ast.stmt_extern), tokens = parse_stmt_extern tl false in
     Ast.Extern stmt, tokens
  | {ttype = Export; _} :: {ttype = Struct; _} :: tl ->
     let stmt, tokens = parse_stmt_struct tl true in
     Ast.Struct stmt, tokens
  | {ttype = Struct; _} :: tl ->
     let stmt, tokens = parse_stmt_struct tl false in
     Ast.Struct stmt, tokens
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
  let rec aux = function
    | [] -> []
    | hd :: _ when hd.Token.ttype = TokenType.Eof -> []
    | tokens' ->
       let stmt, rest = parse_toplvl_stmt tokens' in
       [stmt] @ aux rest in

  aux tokens
