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
  ;;

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
  ;;

  (* Takes a list and discards the head of it
   * but returns the tail of it. Should be used
   * when wanting to discard the head. *)
  let rem (lst : Token.t list) : Token.t list =
    match lst with
    | [] -> let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in exit 1
    | _ :: tl -> tl
  ;;

  (* Takes a list and splits the head from the tail
   * and returns both. Should be used when wanting to
   * consume the head, but still use it. *)
  let pop (lst : Token.t list) : Token.t * Token.t list =
    match lst with
    | [] ->
       let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
       exit 1
    | hd :: tl -> hd, tl
  ;;

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
    | {ttype = TokenType.Identifier; _} as id :: tl -> Ast.Term (Ast.Ident id), tl
    | {ttype = TokenType.IntegerLiteral; _} as intlit :: tl -> Ast.Term (Ast.Intlit intlit), tl
    | {ttype = TokenType.LParen; _} :: tl ->
       let expr, tokens = parse_expr tl in
       let _, tokens = expect tokens TokenType.RParen in
       expr, tokens
    | [] -> let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in exit 1
    | hd :: _ ->
       let _ = Err.err Err.Unknown_token __FILE__ __FUNCTION__ @@ Some hd in
       exit 1

  (* The fourth level of expression parsing. Deals with equality
   * operators `==`, `<`, `>=` etc. *)
  and parse_eq_expr (tokens : Token.t list) : Ast.expr * Token.t list =
    let rec aux (tokens : Token.t list) (lhs : Ast.expr) : Ast.expr * Token.t list =
      match tokens with
      | {ttype = TokenType.DoubleEquals; _}
        | {ttype = TokenType.GreaterThan; _}
        | {ttype = TokenType.LessThan; _} as op :: tl ->
         let (rhs : Ast.expr), tokens = parse_primary_expr tl in
         aux tokens (Binary {lhs; rhs; op})
      | _ -> lhs, tokens
    in
    let lhs, tokens = parse_primary_expr tokens in
    aux tokens lhs

  (* The third level of expression parsing. Deals with multiplicative
   * operators `*`, `/` etc. *)
  and parse_mult_expr (tokens : Token.t list) : Ast.expr * Token.t list =
    let rec aux (tokens : Token.t list) (lhs : Ast.expr) : Ast.expr * Token.t list =
      match tokens with
      | {ttype = TokenType.Asterisk; _}
        | {ttype = TokenType.ForwardSlash; _} as op :: tl ->
         let (rhs : Ast.expr), tokens = parse_eq_expr tl in
         aux tokens (Binary {lhs; rhs; op})
      | _ -> lhs, tokens
    in
    let lhs, tokens = parse_eq_expr tokens in
    aux tokens lhs

  (* The second level of expression parsing. Deals with additive
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

  (* The first level of expression parsing. All expressions
   * that need parsing will call this function. *)
  and parse_expr (tokens : Token.t list) : Ast.expr * Token.t list =
    let expr, tokens = parse_add_expr tokens in
    expr, tokens
  ;;

  let rec parse_block_stmt (tokens : Token.t list) : Ast.block_stmt * Token.t list =
    assert false
  ;;

(*
  proc something(a: i32, b: i32): i32 {
    let s: i32 = 1 + (a*(32/4));
  }
*)

  (* Given a list of tokens, will parse a function definition
   * returning a Ast.node_stmt w/ constructor Ast.node_stmt_block. 
   * Does not need to consume `proc` keyword as the higher order
   * function `parse_stmt` or `parse_toplvl_stmt` already does. *)
  let rec parse_proc_def_stmt (tokens : Token.t list) : Ast.proc_def_stmt * Token.t list =

    let rec aux (tokens : Token.t list) (acc : (Token.t * TokenType.t) list)
            : (Token.t * TokenType.t) list * Token.t list =
      match tokens with
      | {ttype = TokenType.RParen; _} :: tl -> acc, tl
      | {ttype = TokenType.Identifier; _} as id :: tl ->
         let _, tokens = expect tl TokenType.Colon in
         let type_, tokens = expect tokens TokenType.Type in
         let next, tokens = pop tokens in
         let acc = acc @ [id, type_.ttype] in
         (match next with
          | {ttype = TokenType.RParen; _} -> acc, tokens
          | {ttype = TokenType.Comma; _} -> aux tokens acc
          | _ ->
             let _ = Err.err Err.Malformed_func_def __FILE__ __FUNCTION__ None in
             exit 1)
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
    | Some {ttype = TokenType.Void; _} -> failwith "`void` not implemented for proc defs"
    | Some {ttype = TokenType.Identifier; _} ->
       let params, tokens = aux tokens [] in
       let block, tokens = parse_block_stmt tokens in
       Ast.{id; params; block}, tokens
    | Some t ->
       let _ = Err.err Err.Malformed_func_def __FILE__ __FUNCTION__ None in
       exit 1
    | None ->
       let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
       exit 1
  ;;

  let parse_mut_stmt (tokens : Token.t list) : Ast.mut_stmt * Token.t list =
    let id, tokens = expect tokens TokenType.Identifier in
    let _, tokens = expect tokens TokenType.Equals in
    let expr, tokens = parse_expr tokens in
    let _, tokens = expect tokens TokenType.Semicolon in
    Ast.{id; expr}, tokens
  ;;

  (* Parses the statement of `let`. The `let` keyword 
   * has already been consumed by higher order function `parse_stmt`. *)
  let parse_let_stmt (tokens : Token.t list) : Ast.let_stmt * Token.t list =
    let id, tokens = expect tokens TokenType.Identifier in
    let _, tokens = expect tokens TokenType.Colon in
    let type_, tokens = expect tokens TokenType.Type in
    let _, tokens = expect tokens TokenType.Equals in
    let expr, tokens = parse_expr tokens in
    let _, tokens = expect tokens TokenType.Semicolon in
    Ast.{id; type_; expr}, tokens
  ;;

  (* Given a list of tokens, will parse the "outer" statements
   * aka function defs, structs etc. *)
  let parse_stmt (tokens : Token.t list) : Ast.stmt * Token.t list =
    match tokens with
    | {ttype = TokenType.Proc; _} :: tl ->
       let stmt, tokens = parse_proc_def_stmt tokens in
       Proc_def stmt, tokens
    | {ttype = TokenType.Let; _} :: tl ->
       let stmt, tokens = parse_let_stmt tokens in
       Let stmt, tokens
    | {ttype = TokenType.Identifier (* as id *); _} :: tl ->
       let stmt, tokens = parse_mut_stmt tokens in
       Mut stmt, tokens
    | {ttype = TokenType.LBrace; _} :: tl -> assert false
    | hd :: _ ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__
                 ~msg:"unsupported token" @@ Some hd in exit 1
    | _ ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__
                 ~msg:"unsupported token" @@ None in exit 1
  ;;

  let parse_toplvl_stmt (tokens : Token.t list) : Ast.toplvl_stmt * Token.t list =
    match tokens with
    | {ttype = TokenType.Proc; _} :: tl ->
       let stmt, tokens = parse_proc_def_stmt tokens in
       Proc_def stmt, tokens
    | {ttype = TokenType.Let; _} :: tl ->
       let stmt, toknes = parse_let_stmt tokens in
       Let stmt, tokens
    | hd :: _ ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__ ~msg:"invalid top level stmt" @@ Some hd in
       exit 1
    | _ -> 
       let _ = Err.err Err.Exhausted_tokens __FILE__ __FUNCTION__ None in
       exit 1
  ;;

  (* Entrypoint of the parser. Takes a list of tokens and produces
   * a node_prog. *)
  let produce_ast (tokens : Token.t list) : Ast.program =
    let rec aux = function
      | [] -> []
      | hd :: _ when hd.Token.ttype = TokenType.Eof -> []
      | tokens' -> let stmt, rest = parse_toplvl_stmt tokens' in [stmt] @ aux rest in
    aux tokens
  ;;

end
