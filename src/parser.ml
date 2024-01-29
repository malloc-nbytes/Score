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
  let peek (lst : Token.t list) : Token.t option =
    match lst with
    | [] -> None
    | hd :: _ -> Some hd

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
    let (lhs : Ast.expr), tokens = parse_primary_expr tokens in
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
    let (lhs : Ast.expr), tokens = parse_eq_expr tokens in
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
    let (lhs : Ast.expr), tokens = parse_mult_expr tokens in
    aux tokens lhs

  (* The first level of expression parsing. All expressions
   * that need parsing will call this function. *)
  and parse_expr (tokens : Token.t list) : Ast.expr * Token.t list =
    let (expr : Ast.expr), tokens = parse_add_expr tokens in
    expr, tokens
  ;;

  (* Given a list of tokens, will parse a function definition
   * returning a Ast.node_stmt w/ constructor Ast.node_stmt_block. *)
  let rec parse_func_def (tokens : Token.t list) : Ast.stmt * Token.t list =
    assert false
  ;;

  (* Given a list of tokens, will parse the "outer" statements
   * aka function defs, structs etc. *)
  let parse_stmt (tokens : Token.t list) : Ast.stmt * Token.t list =
    match tokens with
    | {ttype = TokenType.Proc; _} :: tl -> failwith "parse_stmt Proc todo"
    | {ttype = TokenType.Let; _} :: tl -> failwith "parse_stmt Let todo"
    | {ttype = TokenType.Identifier (* as id *); _} :: tl -> failwith "parse_stmt Mut todo"
    | {ttype = TokenType.LBrace; _} :: tl -> failwith "parse_stmt Block todo"
    | hd :: _ ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__
         ~msg:"unsupported token" @@ Some hd in exit 1
    | _ ->
       let _ = Err.err Err.Fatal __FILE__ __FUNCTION__
                 ~msg:"unsupported token" @@ None in exit 1

  (* Entrypoint of the parser. Takes a list of tokens and produces
   * a node_prog. *)
  let produce_ast (tokens : Token.t list) : Ast.program =
    let rec aux = function
      | [] -> []
      | hd :: _ when hd.Token.ttype = TokenType.Eof -> []
      | tokens' -> let stmt, rest = parse_stmt tokens' in [stmt] @ aux rest in
    aux tokens
  ;;

end
