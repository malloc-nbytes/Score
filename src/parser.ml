(* TODO:
 *   Factor out the repeating code
 *   of the `aux` inner functions inside
 *   of all the expression parsing functions. *)

module Parser = struct
  open Token
  open Ast
  open Err
  open Printf

  (* Takes some option and attempts
   * to unwrap it, returning the inner value.
   * Will panic if `k` is None. *)
  let unwrap k =
    match k with
    | Some k' -> k'
    | None -> Err.err Err.ParserFatalErr __FILE__ __FUNCTION__ ~msg:"unwrapped None value" None;
              exit 1
  ;;

  (* Takes a list of tokens and an expected token type.
   * If the type of the head of the list does not match `exp`,
   * it will fail. It will also fail if `lst` is empty. Will
   * return the head and tail split from each other. This function
   * should be used instead of `pop ()` when we want to assure a
   * specific type. *)
  let expect (lst : Token.t list) (exp : TokenType.t) : Token.t * Token.t list =
    match lst with
    | [] ->
       Err.err Err.ParserExhaustedTokensErr __FILE__ __FUNCTION__ None;
       exit 1
    | hd :: _ when hd.ttype <> exp ->
       let actual = TokenType.to_string hd.ttype
       and expected = TokenType.to_string exp in
       Err.err Err.ParserExpectErr __FILE__ __FUNCTION__
         ~msg:(sprintf "expected %s but got %s" expected actual) @@ Some hd;
       exit 1
    | hd :: tl -> hd, tl
  ;;

  (* Takes a list and discards the head of it
   * but returns the tail of it. Should be used
   * when wanting to discard the head. *)
  let rem (lst : Token.t list) : Token.t list =
    match lst with
    | [] -> Err.err Err.ParserExhaustedTokensErr __FILE__ __FUNCTION__ None; exit 1
    | _ :: tl -> tl
  ;;

  (* Takes a list and splits the head from the tail
   * and returns both. Should be used when wanting to
   * consume the head, but still use it. *)
  let pop (lst : Token.t list) : Token.t * Token.t list =
    match lst with
    | [] ->
       Err.err Err.ParserExhaustedTokensErr __FILE__ __FUNCTION__ None;
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
  let rec parse_primary_expr (tokens : Token.t list) : Ast.node_expr * Token.t list =
    match tokens with
    | {ttype = TokenType.Identifier; _} as id :: tl -> Ast.NodeTerm (NodeTermID {id}), tl
    | {ttype = TokenType.IntegerLiteral; _} as intlit :: tl -> Ast.NodeTerm (NodeTermIntLit {intlit}), tl
    | {ttype = TokenType.LParen; _} :: tl ->
       let expr, tokens = parse_expr tl in
       let _, tokens = expect tokens TokenType.RParen in
       expr, tokens
    | [] -> Err.err Err.ParserExhaustedTokensErr __FILE__ __FUNCTION__ None; exit 1
    | hd :: _ ->
       Err.err Err.ParserUnknownTokenErr __FILE__ __FUNCTION__ @@ Some hd;
       exit 1

  (* The fourth level of expression parsing. Deals with equality
   * operators `==`, `<`, `>=` etc. *)
  and parse_eq_expr (tokens : Token.t list) : Ast.node_expr * Token.t list =
    let rec aux (tokens : Token.t list) (lhs : Ast.node_expr) : Ast.node_expr * Token.t list =
      match tokens with
      | {ttype = TokenType.DoubleEquals; _} as op :: tl ->
         let (rhs : Ast.node_expr), tokens = parse_primary_expr tl in
         aux tokens (NodeBinExpr {lhs; rhs; op = op.value})
      | _ -> lhs, tokens
    in
    let (lhs : Ast.node_expr), tokens = parse_primary_expr tokens in
    aux tokens lhs

  (* The third level of expression parsing. Deals with multiplicative
   * operators `*`, `/` etc. *)
  and parse_mult_expr (tokens : Token.t list) : Ast.node_expr * Token.t list =
    let rec aux (tokens : Token.t list) (lhs : Ast.node_expr) : Ast.node_expr * Token.t list =
      match tokens with
      | {ttype = TokenType.Asterisk; _}
        | {ttype = TokenType.ForwardSlash; _} as op :: tl ->
         let (rhs : Ast.node_expr), tokens = parse_eq_expr tl in
         aux tokens (NodeBinExpr {lhs; rhs; op = op.value})
      | _ -> lhs, tokens
    in
    let (lhs : Ast.node_expr), tokens = parse_eq_expr tokens in
    aux tokens lhs

  (* The second level of expression parsing. Deals with additive
   * operators `+`, `-` etc. *)
  and parse_add_expr (tokens : Token.t list) : Ast.node_expr * Token.t list =
    let rec aux (tokens : Token.t list) (lhs : Ast.node_expr) : Ast.node_expr * Token.t list =
      match tokens with
      | {ttype = TokenType.Plus; _}
        | {ttype = TokenType.Minus; _} as op :: tl ->
         let (rhs : Ast.node_expr), tokens = parse_mult_expr tl in
         aux tokens (NodeBinExpr {lhs; rhs; op = op.value})
      | _ -> lhs, tokens
    in
    let (lhs : Ast.node_expr), tokens = parse_mult_expr tokens in
    aux tokens lhs

  (* The first level of expression parsing. All expressions
   * that need parsing will call this function. *)
  and parse_expr (tokens : Token.t list) : Ast.node_expr * Token.t list =
    let (expr : Ast.node_expr), tokens = parse_add_expr tokens in
    expr, tokens
  ;;

  (* Given a list of tokens, will parse a block statement
   * aka { ... }. This function is used when parsing statements
   * inside of a function (let, if, while etc). *)
  let rec parse_block_stmt (tokens : Token.t list) (acc : Ast.node_stmt list)
          : Ast.node_stmt_block * Token.t list =
    match tokens with
    | [] -> Err.err Err.ParserExhaustedTokensErr __FILE__ __FUNCTION__ None; exit 1
    | {ttype = TokenType.RBrace; _} :: tl -> Ast.{stmts = acc}, tl
    | {ttype = TokenType.Let; _} :: tl ->
       let id, tokens = expect tl TokenType.Identifier in
       let _, tokens = expect tokens TokenType.Colon in
       let vtype, tokens = expect tokens TokenType.Type in
       let _, tokens = expect tokens TokenType.Equals in
       let expr, tokens = parse_expr tokens in
       let _, tokens = expect tokens TokenType.Semicolon in
       parse_block_stmt tokens (acc @ [Ast.NodeStmtLet {id = id.value; expr; mut = true}])
    | {ttype = TokenType.Identifier; value = id} :: tl ->
       let id, tokens = pop tokens in
       let _, tokens = expect tokens TokenType.Equals in
       let expr, tokens = parse_expr tokens in
       let _, tokens = expect tokens TokenType.Semicolon in
       parse_block_stmt tokens (acc @ [Ast.NodeStmtMut {id = id.value; expr}])
    | hd :: _ ->
       Err.err Err.ParserUnknownTokenErr __FILE__ __FUNCTION__ @@ Some hd;
       exit 1
  ;;

  (* Given a list of tokens, will parse a function definition
   * returning a Ast.node_stmt w/ constructor Ast.node_stmt_block. *)
  let parse_func_def (tokens : Token.t list) : Ast.node_stmt * Token.t list =
    let rec gather_params (tokens : Token.t list) (acc : (string * TokenType.t) list)
            : ((string * TokenType.t) list) * Token.t list =
      match tokens with
      | {ttype = TokenType.RParen; _} :: tl -> acc, tl
      | {ttype = TokenType.Identifier; _} as id :: tl ->
         let _, tokens = expect tl TokenType.Colon in
         let ptype, tokens = expect tokens TokenType.Type in
         let next, tokens = pop tokens in
         let acc = acc @ [id.value, ptype.ttype] in
         (match next with
          | {ttype = TokenType.RParen; _} -> acc, tokens
          | {ttype = TokenType.Comma; _} -> gather_params tokens @@ acc
          | _ -> Err.err Err.ParserMalformedFuncDef __FILE__ __FUNCTION__ None; exit 1)
      | hd :: _ -> Err.err Err.ParserMalformedFuncDef __FILE__ __FUNCTION__ None; exit 1
      | [] ->
         Err.err Err.ParserMalformedFuncDef
           __FILE__ __FUNCTION__ ~msg:"unterminated function definition" None;
         exit 1
    in

    let func_name, tokens = expect tokens TokenType.Identifier in
    let _, tokens = expect tokens TokenType.LParen in
    let (params : (string * TokenType.t) list), tokens = gather_params tokens [] in
    let _, tokens = expect tokens TokenType.Colon in
    let rtype, tokens = expect tokens TokenType.Type in
    let _, tokens = expect tokens TokenType.LBrace in

    let block_stmt, tokens = parse_block_stmt tokens [] in
    NodeStmtFuncDef {id = func_name.value; params; rtype; block_stmt}, tokens
  ;;

  (* Given a list of tokens, will parse the "outer" statements
   * aka function defs, structs etc. *)
  let parse_primary_stmt (tokens : Token.t list) : Ast.node_stmt * Token.t list =
    match tokens with
    | {ttype = TokenType.Proc; _} :: tl -> parse_func_def tl
    | hd :: _ ->
       Err.err Err.ParserUnknownTokenErr __FILE__ __FUNCTION__ @@ Some hd;
       exit 1
    | [] ->
       Err.err Err.ParserExhaustedTokensErr __FILE__ __FUNCTION__ None;
       exit 1
  ;;

  (* Entrypoint of the parser. Takes a list of tokens and produces
   * a node_prog. *)
  let produce_ast (tokens : Token.t list) : Ast.node_prog =
    let rec aux = function
      | [] -> []
      | hd :: _ when hd.Token.ttype = TokenType.Eof -> []
      | tokens' -> let stmt, rest = parse_primary_stmt tokens' in [stmt] @ aux rest in
    Ast.{stmts = aux tokens}
  ;;

end
