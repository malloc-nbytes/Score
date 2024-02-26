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

(* TODO:
 *   Factor out the repeating code
 *   of the `aux` inner functions inside
 *   of all the expression parsing functions. *)

module Parser = struct
  open Token
  open Ast
  open Err

  (* Takes some option and attempts
   * to unwrap it, returning the inner value.
   * Will panic if `k` is None. *)
  let unwrap k =
    match k with
    | Some k' -> k'
    | None ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"unwrapped None value" None in
       exit 1

  (* Takes a list of tokens and an expected token type.
   * If the type of the head of the list does not match `exp`,
   * it will fail. It will also fail if `lst` is empty. Will
   * return the head and tail split from each other. This function
   * should be used instead of `pop ()` when we want to assure a
   * specific type. *)
  let expect (lst : Token.t list) (exp : TokenType.t) : Token.t * Token.t list =
    let open Printf in
    match lst with
    | [] ->
       let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
       exit 1
    | hd :: _ when hd.ttype <> exp ->
       let actual = TokenType.to_string hd.ttype
       and expected = TokenType.to_string exp in
       let _ = Err.err Err.Expect __FILE__ __FUNCTION__
                 ~msg:(sprintf "expected %s but got %s" expected actual) @@ Some hd in exit 1
    | hd :: tl -> hd, tl

  (* Takes a list and discards the head of it
   * but returns the tail of it. Should be used
   * when wanting to discard the head. *)
  let rem (lst : Token.t list) : Token.t list =
    match lst with
    | [] -> let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in exit 1
    | _ :: tl -> tl

  (* Takes a list and splits the head from the tail
   * and returns both. Should be used when wanting to
   * consume the head, but still use it. *)
  let pop (lst : Token.t list) : Token.t * Token.t list =
    match lst with
    | [] ->
       let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
       exit 1
    | hd :: tl -> hd, tl

  (* Takes a token list and will peek the top
   * token. If something exists, return Some (`hd`).
   * Otherwise, return None. *)
  let rec peek (lst : Token.t list) (i : int) : Token.t option =
    match i, lst with
    | 0, hd :: _ -> Some hd
    | k, _ :: tl -> peek tl (k - 1)
    | _ -> None

  (* The last level of expression parsing. Checks for an
   * identifier, integer literal etc. If a left paren `(` is
   * encountered, it will recurse back to the first level of
   * expression parsing and work that sub-expression back up
   * to this level. *)
  let rec parse_primary_expr (tokens : Token.t list) : Ast.expr * Token.t list =
    match tokens with
    | {ttype = TokenType.Identifier; _} as id :: tl ->
       (match peek tl 0 with
        | Some {ttype = TokenType.LParen; _} -> (* Procedure call *)
           let proc_call, tokens = parse_proc_call (id :: tl) in
           Ast.Proc_call proc_call, tokens
        | _ -> Ast.Term (Ast.Ident id), tl) (* Variable *)
    | {ttype = TokenType.IntegerLiteral; _} as intlit :: tl -> Ast.Term (Ast.Intlit intlit), tl
    | {ttype = TokenType.StringLiteral; _} as strlit :: tl -> Ast.Term (Ast.Strlit strlit), tl
    | {ttype = TokenType.LParen; _} :: tl ->
       let expr, tokens = parse_expr tl in
       let _, tokens = expect tokens TokenType.RParen in
       expr, tokens
    | [] -> let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in exit 1
    | hd :: _ ->
       let _ = Err.err Err.Unknown_token __FILE__ __FUNCTION__ @@ Some hd in
       exit 1

  (* The fifth level of expression parsing. Deals with multiplicative
   * operators `*`, `/` etc. *)
  and parse_mult_expr (tokens : Token.t list) : Ast.expr * Token.t list =
    let rec aux (tokens : Token.t list) (lhs : Ast.expr) : Ast.expr * Token.t list =
      match tokens with
      | {ttype = TokenType.Asterisk; _}
        | {ttype = TokenType.ForwardSlash; _}
          | {ttype = TokenType.Percent; _} as op :: tl ->
         let (rhs : Ast.expr), tokens = parse_primary_expr tl in
         aux tokens (Binary {lhs; rhs; op})
      | _ -> lhs, tokens
    in
    let lhs, tokens = parse_primary_expr tokens in
    aux tokens lhs

  (* The fourth level of expression parsing. Deals with additive
   * operators `+`, `-` etc. *)
  and parse_add_expr (tokens : Token.t list) : Ast.expr * Token.t list =
    let rec aux (tokens : Token.t list) (lhs : Ast.expr) : Ast.expr * Token.t list =
      match tokens with
      | {ttype = TokenType.Plus; _}
        | {ttype = TokenType.Minus; _} as op :: tl ->
         let (rhs : Ast.expr), tokens = parse_mult_expr tl in
         aux tokens (Binary {lhs; rhs; op})
      | _ -> lhs, tokens
    in
    let lhs, tokens = parse_mult_expr tokens in
    aux tokens lhs

  (* The third level of expression parsing. Deals with equality
   * operators `==`, `<`, `>=` etc. *)
   and parse_eq_expr (tokens : Token.t list) : Ast.expr * Token.t list =
      let rec aux (tokens : Token.t list) (lhs : Ast.expr) : Ast.expr * Token.t list =
         match tokens with
         | {ttype = TokenType.DoubleEquals; _}
            | {ttype = TokenType.GreaterThan; _}
            | {ttype = TokenType.GreaterThanEqual; _}
            | {ttype = TokenType.LessThanEqual; _}
            | {ttype = TokenType.NotEqual; _}
            | {ttype = TokenType.LessThan; _} as op :: tl ->
            let (rhs : Ast.expr), tokens = parse_add_expr tl in
            aux tokens (Binary {lhs; rhs; op})
         | _ -> lhs, tokens
      in
      let lhs, tokens = parse_add_expr tokens in
      aux tokens lhs

  (* The second level of expression parsing. Deals with logical
   * operators `&&`, `||` etc. *)
  and parse_logical_expr (tokens : Token.t list) : Ast.expr * Token.t list =
    let rec aux (tokens : Token.t list) (lhs : Ast.expr) : Ast.expr * Token.t list =
      match tokens with
      | {ttype = TokenType.DoubleAmpersand; _} as op :: tl ->
         let (rhs : Ast.expr), tokens = parse_eq_expr tl in
         aux tokens (Binary {lhs; rhs; op})
      | _ -> lhs, tokens
    in
    let lhs, tokens = parse_eq_expr tokens in
    aux tokens lhs

  (* The first level of expression parsing. All expressions
   * that need parsing will call this function. *)
  and parse_expr (tokens : Token.t list) : Ast.expr * Token.t list =
    let expr, tokens = parse_logical_expr tokens in
    expr, tokens

  (* Parses the block statement aka `{...}`. Does not consume
   * `{`, as it is the job of the caller. *)
  and parse_block_stmt (tokens : Token.t list) : Ast.block_stmt * Token.t list =
    let rec aux (tokens : Token.t list) (acc : Ast.stmt list) : Ast.stmt list * Token.t list =
      match tokens with
      | [] ->
         let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
         exit 1
      | {ttype = TokenType.RBrace; _} :: tl -> acc, tl
      | lst ->
         let stmt, tokens = parse_stmt lst in
         aux tokens @@ acc @ [stmt] in
    let stmts, tokens = aux tokens [] in
    Ast.{stmts}, tokens

  (* Given a list of tokens, will parse a function definition
   * returning an Ast.node_stmt w/ constructor Ast.node_stmt_block.
   * Does not need to consume `proc` keyword as the caller
   * function `parse_stmt` or `parse_toplvl_stmt` already does. *)
  and parse_proc_def_stmt (tokens : Token.t list) : Ast.proc_def_stmt * Token.t list =
    let rec gather_params (tokens : Token.t list) (acc : (Token.t * Token.t) list)
            : (Token.t * Token.t) list * Token.t list =
      match tokens with
      | {ttype = TokenType.RParen; _} :: tl -> acc, tl
      | {ttype = TokenType.Identifier; _} as id :: tl ->
         let _, tokens = expect tl TokenType.Colon in
         let type_, tokens = expect tokens TokenType.Type in
         let next, tokens = pop tokens in
         let acc = acc @ [id, type_] in
         (match next with
          | {ttype = TokenType.RParen; _} -> acc, tokens
          | {ttype = TokenType.Comma; _} -> gather_params tokens acc
          | _ ->
             let _ = Err.err Err.Malformed_func_def __FILE__ __FUNCTION__ None in
             exit 1)
      | {ttype = TokenType.Void; _} :: {ttype = TokenType.RParen; _} :: tl -> [], tl
      | hd :: _ ->
         let _ = Err.err Err.Malformed_func_def __FILE__ __FUNCTION__ @@ Some hd in
         exit 1
      | [] ->
         let _ = Err.err Err.Malformed_func_def __FILE__ __FUNCTION__
                   ~msg:"unterminated function definition" None in
         exit 1 in

    let id, tokens = expect tokens TokenType.Identifier in
    let _, tokens = expect tokens TokenType.LParen in

    match peek tokens 0 with
    | Some {ttype = TokenType.Identifier; _} | Some {ttype = TokenType.Void; _} ->
       let params, tokens = gather_params tokens [] in  (* Consumes `)` *)
       let _, tokens = expect tokens TokenType.Colon in
       let rettype, tokens = expect tokens TokenType.Type in
       let _, tokens = expect tokens TokenType.LBrace in
       let block, tokens = parse_block_stmt tokens in
       Ast.{id; params; block; rettype = rettype.ttype}, tokens
    | Some t ->
       let _ = Err.err Err.Malformed_func_def __FILE__ __FUNCTION__ None in
       exit 1
    | None ->
       let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
       exit 1

  (* Used when parsing a statement of mutating a value
   * that is already declared. *)
  and parse_mut_stmt (tokens : Token.t list) : Ast.mut_stmt * Token.t list =
    let id, tokens = expect tokens TokenType.Identifier in
    let _, tokens = expect tokens TokenType.Equals in
    let expr, tokens = parse_expr tokens in
    let _, tokens = expect tokens TokenType.Semicolon in
    Ast.{id; expr}, tokens

  (* Parses the statement of `let`. The `let` keyword
   * has already been consumed by higher order function `parse_stmt`. *)
  and parse_let_stmt (tokens : Token.t list) : Ast.let_stmt * Token.t list =
    let id, tokens = expect tokens TokenType.Identifier in
    let _, tokens = expect tokens TokenType.Colon in
    let type_, tokens = expect tokens TokenType.Type in
    let _, tokens = expect tokens TokenType.Equals in
    let expr, tokens = parse_expr tokens in
    let _, tokens = expect tokens TokenType.Semicolon in
    Ast.{id; type_; expr}, tokens

  (* Parses the if statement. *)
  and parse_if_stmt (tokens : Token.t list) : Ast.if_stmt * Token.t list =
    let expr, tokens = parse_expr tokens in
    let _, tokens = expect tokens TokenType.LBrace in
    let block, tokens = parse_block_stmt tokens in
    (match tokens with
     | {ttype = TokenType.Else; _} :: ({ttype = TokenType.If; _}) :: tl2 ->
        failwith "if else if unimplemented"
     (* let tokens = tl1 :: tl2 in
        let else_, tokens = parse_block_stmt tokens in
        Ast.{expr; block; else_ = Some else_}, tokens *)
     | {ttype = TokenType.Else; _} :: tl ->
        let _, tokens = expect tl TokenType.LBrace in
        let else_, tokens = parse_block_stmt tokens in
        Ast.{expr; block; else_ = Some else_}, tokens
     | _ -> Ast.{expr; block; else_ = None}, tokens)

  (* Parses the while statement. *)
  and parse_while_stmt (tokens : Token.t list) : Ast.while_stmt * Token.t list =
    let expr, tokens = parse_expr tokens in
    let _, tokens = expect tokens TokenType.LBrace in
    let block, tokens = parse_block_stmt tokens in
    Ast.{expr; block}, tokens

  (* Parses the stmt_expr of a procedure call. *)
  and parse_proc_call (tokens : Token.t list) : Ast.proc_call_expr * Token.t list =
    let rec parse_args (tokens : Token.t list) (acc : Ast.expr list) : Ast.expr list * Token.t list =
      match tokens with
      | {ttype = TokenType.RParen; _} :: tl -> acc, tokens (* Case for no args *)
      | tokens ->
         let expr, tokens = parse_expr tokens in
         let acc = acc @ [expr] in
         (match peek tokens 0 with
          | Some {ttype = TokenType.Comma; _} -> parse_args (List.tl tokens) acc
          | _ -> acc, tokens) in
    let id, tokens = expect tokens TokenType.Identifier in
    let _, tokens = expect tokens TokenType.LParen in
    let args, tokens = parse_args tokens [] in
    let _, tokens = expect tokens TokenType.RParen in
    (* let _, tokens = expect tokens TokenType.Semicolon in *)
    Ast.{id; args}, tokens

  and parse_ret_stmt (tokens : Token.t list) : Ast.ret_stmt * Token.t list =
    let expr, tokens = parse_expr tokens in
    let _, tokens = expect tokens TokenType.Semicolon in
    Ast.{expr}, tokens

  (* Given a list of tokens, will parse the "outer" statements
   * aka function defs, structs etc. *)
  and parse_stmt (tokens : Token.t list) : Ast.stmt * Token.t list =
    match tokens with
    | {ttype = TokenType.Proc; _} :: tl ->
       let stmt, tokens = parse_proc_def_stmt tl in
       Proc_def stmt, tokens
    | {ttype = TokenType.Let; _} :: tl ->
       let stmt, tokens = parse_let_stmt tl in
       Let stmt, tokens
    | {ttype = TokenType.Identifier; _} as hd :: tl ->
       (match peek tl 0 with
        | Some {ttype = TokenType.LParen; _} -> (* Procedure call *)
           let stmt, tokens = parse_proc_call (hd :: tl) in
           let _, tokens = expect tokens TokenType.Semicolon in
           Ast.Stmt_expr (Ast.Proc_call stmt), tokens
        | Some _ -> (* Mutating variable *)
           let stmt, tokens = parse_mut_stmt (hd :: tl) in
           Mut stmt, tokens
        | _ ->
           let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
           exit 1)
    | {ttype = TokenType.If; _} :: tl ->
       let stmt, tokens = parse_if_stmt tl in
       If stmt, tokens
    | {ttype = TokenType.While; _} :: tl ->
       let stmt, tokens = parse_while_stmt tl in
       While stmt, tokens
    | {ttype = TokenType.Return; _} :: tl ->
       let stmt, tokens = parse_ret_stmt tl in
       Ret stmt, tokens
    | hd :: _ ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__
                 ~msg:"unsupported token" @@ Some hd in exit 1
    | _ ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__
                 ~msg:"unsupported token" @@ None in exit 1

  (* Parses the top-most statements (proc decls, global vars etc). *)
  let parse_toplvl_stmt (tokens : Token.t list) : Ast.toplvl_stmt * Token.t list =
    match tokens with
    | {ttype = TokenType.Proc; _} :: tl ->
       let stmt, tokens = parse_proc_def_stmt tl in
       Proc_def stmt, tokens
    | {ttype = TokenType.Let; _} :: tl ->
       let stmt, toknes = parse_let_stmt tl in
       Let stmt, tokens
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
      | tokens' -> let stmt, rest = parse_toplvl_stmt tokens' in [stmt] @ aux rest in
    aux tokens

end
