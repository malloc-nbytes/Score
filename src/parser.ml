(* TODO:
 *   Factor out the repeating code
 *   of the `aux` inner functions inside
 *   of all the expression parsing functions. *)

module Parser = struct
  open Token
  open Ast

  (* Takes some option and attempts
   * to unwrap it, returning the inner value.
   * Will panic if `k` is None. *)
  let unwrap k =
    match k with
    | Some k' -> k'
    | None -> failwith "unwrapped None value"
  ;;

  (* Takes a token and returns a string in a syntax error-like message. *)
  let syntax_error (token : Token.t) : string =
    let value, ttype, line, ch = token.Token.value, TokenType.to_string token.ttype, token.r, token.c in
    Printf.sprintf "\nSYNTAX ERROR\n(line %d, char %d, %s \"%s\")" line ch ttype value

  (* Takes a list of tokens and an expected token type.
   * If the type of the head of the list does not match `exp`,
   * it will fail. It will also fail if `lst` is empty. Will
   * return the head and tail split from each other. This function
   * should be used instead of `pop ()` when we want to assure a
   * specific type *)
  let expect (lst : Token.t list) (exp : TokenType.t) : Token.t * Token.t list =
    match lst with
    | []                           -> failwith "call to expect () with an empty list"
    | hd :: _ when hd.ttype <> exp ->
       let actual = TokenType.to_string hd.ttype
       and expected = TokenType.to_string exp
       and se = syntax_error hd in
       let _ = Printf.printf "%s\nexpected %s but got %s" se expected actual in
       exit 1
    | hd :: tl -> hd, tl
  ;;

  (* Takes a list and discards the head of it
   * but returns the tail of it. Should be used
   * when wanting to discard the head. *)
  let rem (lst : Token.t list) : Token.t list =
    match lst with
    | []      -> failwith "called rem () with no tokens"
    | _ :: tl -> tl
  ;;

  (* Takes a list and splits the head from the tail
   * and returns both. Should be used when wanting to
   * consume the head, but still use it. *)
  let pop (lst : Token.t list) : Token.t * Token.t list =
    match lst with
    | []       -> failwith "called pop () with no tokens"
    | hd :: tl -> hd, tl
  ;;

  (* Takes a token list and will peek the top
   * token. If something exists, return Some (`hd`).
   * Otherwise, return None. *)
  let peek (lst : Token.t list) : Token.t option =
    match lst with
    | []      -> None
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
    | [] -> failwith "parse_primary_expr () failed with no tokens"
    | _ -> failwith @@ Printf.sprintf "parse_primary_expr () failed. Unknown token: %s"
                         (unwrap (peek tokens)).Token.value

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
      | {ttype = TokenType.Asterisk; _} | {ttype = TokenType.ForwardSlash; _} as op :: tl ->
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
      | {ttype = TokenType.Plus; _} | {ttype = TokenType.Minus; _} as op :: tl ->
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

  (* Given a list of tokens, will parse a compound statement
   * aka { ... }. This function is used when parsing statements
   * inside of a function (let, if, while etc). *)
  let rec parse_compound_stmt (tokens : Token.t list) (acc : Ast.node_stmt list)
      : Ast.node_stmt_compound * Token.t list =
    match tokens with
    | [] -> failwith "parse_compound_stmt () failed with no tokens"
    | {ttype = TokenType.RBrace; _} :: tl -> Ast.{stmts = acc}, tl
    | {ttype = TokenType.Let; _} :: tl ->
       let id, tokens = expect tl TokenType.Identifier in
       let _, tokens = expect tokens TokenType.Colon in
       let vtype, tokens = expect tokens TokenType.Type in
       let _, tokens = expect tokens TokenType.Equals in
       let expr, tokens = parse_expr tokens in
       let _, tokens = expect tokens TokenType.Semicolon in
       parse_compound_stmt tokens (acc @ [Ast.NodeStmtLet {id = id.value; expr; mut = true}])
    | {ttype = TokenType.Identifier; value = id} as hd :: tl ->
       let id, tokens = pop tokens in
       let _, tokens = expect tokens TokenType.Equals in
       let expr, tokens = parse_expr tokens in
       let _, tokens = expect tokens TokenType.Semicolon in
       parse_compound_stmt tokens (acc @ [Ast.NodeStmtMut {id = id.value; expr}])
    | _ -> failwith "parse_compound_stmt () failed with unsupported token"
  ;;

  (* Given a list of tokens, will parse a function definition
   * returning a Ast.node_stmt w/ constr. Ast.node_stmt_compound. *)
  let parse_func_def (tokens : Token.t list) : Ast.node_stmt * Token.t list =
    let rec gather_params (tokens : Token.t list) (acc : (string * TokenType.t) list)
            : ((string * TokenType.t) list) * Token.t list =
      match tokens with
      | {ttype = TokenType.RParen; _} :: tl           -> acc, tl
      | {ttype = TokenType.Identifier; _} as id :: tl ->
         let _, tokens = expect tl TokenType.Colon in
         let ptype, tokens = expect tokens TokenType.Type in
         let next, tokens = pop tokens in
         let acc = acc @ [id.value, ptype.ttype] in
         (match next with
          | {ttype = TokenType.RParen; _} -> acc, tokens
          | {ttype = TokenType.Comma; _}  -> gather_params tokens @@ acc
          | _                             -> failwith "malformed function params")
      | _ -> failwith "invalid func params"
    in

    let func_name, tokens = expect tokens TokenType.Identifier in
    let _, tokens = expect tokens TokenType.LParen in
    let (params : (string * TokenType.t) list), tokens = gather_params tokens [] in
    let _, tokens = expect tokens TokenType.Colon in
    let rtype, tokens = expect tokens TokenType.Type in
    let _, tokens = expect tokens TokenType.LBrace in

    let compound_stmt, tokens = parse_compound_stmt tokens [] in
    NodeStmtFuncDef {id = func_name.value; params; rtype; compound_stmt}, tokens
  ;;

  (* Given a list of tokens, will parse the "outer" statements
   * aka function defs, structs etc. *)
  let parse_primary_stmt (tokens : Token.t list) : Ast.node_stmt * Token.t list =
    match tokens with
    | {ttype = TokenType.Proc; _} :: tl -> parse_func_def tl
    | _ -> failwith "parse_primary_stmt () unsupported token"
  ;;

  (* Entrypoint of the parser. Takes a list of tokens and produces
   * a node_prog. *)
  let produce_ast (tokens : Token.t list) : Ast.node_prog =
    let rec f = function
      | []                                          -> []
      | hd :: _ when hd.Token.ttype = TokenType.Eof -> []
      | tokens'                                     ->
         let stmt, rest = parse_primary_stmt tokens' in
         [stmt] @ f rest in
    Ast.{stmts = f tokens}
  ;;

  (* Takes a node_prog and prints out the AST created. *)
  let print_ast (prog : Ast.node_prog) : unit =
    let rec print_stmt (stmt : Ast.node_stmt) : unit =
      match stmt with
      | Ast.NodeStmtFuncDef {id; params; rtype; compound_stmt} ->
         let _ = Printf.printf "func %s (" id in
         let _ = List.iter (fun (id, ttype) -> Printf.printf "%s: %s, " id (TokenType.to_string ttype)) params in
         let _ = Printf.printf "): %s {\n" (TokenType.to_string rtype.ttype) in
         let _ = List.iter print_stmt compound_stmt.stmts in
         Printf.printf "}\n"
      | Ast.NodeStmtFuncCall {id; args} ->
         let _ = Printf.printf "%s(" id in
         let _ = List.iter (fun arg -> print_expr arg) args in
         Printf.printf ");\n"
      | Ast.NodeStmtCompound {stmts} ->
         let _ = List.iter print_stmt stmts in
         ()
      | Ast.NodeStmtLet {id; expr; mut} ->
         let _ = Printf.printf "let %s = " id in
         let _ = print_expr expr in
         let _ = Printf.printf ";\n" in
         ()
      | Ast.NodeStmtMut {id; expr} ->
         let _ = Printf.printf "%s = " id in
         let _ = print_expr expr in
         let _ = Printf.printf ";\n" in
         ()

    and print_expr (expr : Ast.node_expr) : unit =
      match expr with
      | Ast.NodeBinExpr {lhs; rhs; op} ->
          let _ = print_expr lhs in
          let _ = Printf.printf " %s " op in
          print_expr rhs
      | Ast.NodeTerm t ->
          print_term t
  
    and print_term (term : Ast.node_term) : unit =
      match term with
      | Ast.NodeTermID {id} ->
          Printf.printf "%s" id.value
      | Ast.NodeTermIntLit {intlit} ->
          Printf.printf "%s" intlit.value
  
    in
    List.iter print_stmt prog.stmts

end
