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

module Ir = struct
  open Ast
  open Printf
  open Token
  open Err

  let scope : (((string, (Token.t * (TokenType.id_type option))) Hashtbl.t) list) ref = ref @@ Hashtbl.create 20 :: []

  let func_section   = ref ""
  let data_section   = ref ""

  let tmpreg         = ref ""
  let tmpreg_c       = ref 0

  let if_lbl         = ref ""
  let else_lbl       = ref ""
  let if_done_lbl    = ref ""
  let if_c           = ref 0

  let loop_lbl       = ref ""
  let loop_start_lbl = ref ""
  let loop_end_lbl   = ref ""
  let loop_c         = ref 0

  let didret         = ref false (* TODO: Find a better solution *)
  let didbreak       = ref false (* TODO: Find a better solution *)

  let cur_proc_id     = ref ""

  let push_scope () : unit = scope := Hashtbl.create 20 :: !scope

  let pop_scope () : unit = scope := List.tl !scope

  let assert_id_not_in_scope (token : Token.t) : unit =
    if List.exists (fun s -> Hashtbl.mem s token.lexeme) !scope then
      let _ = Err.err Err.Redeclaration __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "redeclared identifier `%s`" token.lexeme)
                (Some token) in exit 1

  let assert_id_in_scope (token : Token.t) : unit =
    if not (List.exists (fun s -> Hashtbl.mem s token.lexeme) !scope) then
      let _ = Err.err Err.Undeclared __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "undeclared identifier `%s`" token.lexeme)
                (Some token) in exit 1

  let add_id_to_scope (id : string) (token : Token.t) (type_ : TokenType.id_type option) : unit =
    let s = List.hd !scope in
    Hashtbl.add s id (token, type_)

  let get_token_from_scope (id : string) : Token.t * (TokenType.id_type option) =
    let rec get_token_from_scope' (scope : ((string, (Token.t * (TokenType.id_type option))) Hashtbl.t) list) : Token.t * (TokenType.id_type option) =
      match scope with
      | [] -> failwith "unreachable"
      | s :: ss ->
         if Hashtbl.mem s id then
           Hashtbl.find s id
         else
           get_token_from_scope' ss in
    get_token_from_scope' !scope

  (* Construct a `if` label. *)
  let cons_if_lbl () : string * string * string =
    let tmp = string_of_int !if_c in
    if_c := !if_c + 1;
    if_lbl := "@if" ^ tmp;
    else_lbl := "@else" ^ tmp;
    if_done_lbl := "@done" ^ tmp;
    !if_lbl, !else_lbl, !if_done_lbl

  (* Construct a `loop` label. *)
  let cons_loop_lbl () : string * string * string =
    let tmp = string_of_int !loop_c in
    loop_lbl := "@loop" ^ tmp;
    loop_start_lbl := "@loop_start" ^ tmp;
    loop_end_lbl := "@loop_end" ^ tmp;
    loop_c := !loop_c + 1;
    !loop_lbl, !loop_start_lbl, !loop_end_lbl

  (* Construct a `__SCORE_REG` label. *)
  let cons_tmpreg (global : bool) : string =
    let tmp = !tmpreg_c in
    tmpreg_c := !tmpreg_c + 1;
    tmpreg := (if global then "$" else "%") ^ "__SCORE_REG" ^ (string_of_int tmp);
    !tmpreg

  (* Convert the Score datatype to a QBE type. *)
  let scoretype_to_qbetype (type_ : TokenType.id_type) : string =
    match type_ with
    | TokenType.Void -> ""
    | TokenType.I32 -> "w"
    | TokenType.Str -> "l"
    | _ ->
       let _ = Err.err Err.Unimplemented __FILE__ __FUNCTION__
                 ~msg:(Printf.sprintf "unsupported type `%s`" @@ TokenType.id_type_to_string type_)
                 None in exit 1

  (* Get the QBE instruction for a binary operation. *)
  let evaluate_binop (op : Token.t) : string =
    match op.ttype with
    | TokenType.Plus -> "add"
    | TokenType.Minus -> "sub"
    | TokenType.Asterisk -> "mul"
    | TokenType.ForwardSlash -> "div"
    | TokenType.DoubleEquals -> "ceqw"
    | TokenType.LessThan -> "csltw"
    | TokenType.LessThanEqual -> "cslew"
    | TokenType.GreaterThanEqual -> "csgew"
    | TokenType.GreaterThan -> "csgtw"
    | TokenType.NotEqual -> "cnew"
    | TokenType.Percent -> "rem"
    | TokenType.DoubleAmpersand -> "and"
    | TokenType.DoublePipe -> "or"
    | TokenType.PlusEquals -> "add"
    | TokenType.MinusEquals -> "sub"
    | TokenType.AsteriskEquals -> "mul"
    | TokenType.ForwardSlashEquals -> "div"
    | TokenType.PercentEquals -> "rem"
    | _ ->
       let _ = Err.err Err.Unimplemented __FILE__ __FUNCTION__
                 ~msg:(Printf.sprintf "unsupported binop `%s`" op.lexeme)
                 None in exit 1

  (* Evaluate an expression. *)
  let rec evaluate_expr (expr : Ast.expr) : string =
    match expr with
    | Ast.Binary bin ->
       let lhs = evaluate_expr bin.lhs in
       let rhs = evaluate_expr bin.rhs in
       func_section := sprintf "%s    %s =w %s %s, %s\n" !func_section (cons_tmpreg false)
                         (evaluate_binop bin.op) lhs rhs;
       !tmpreg
    | Ast.Term Ast.Ident ident ->
      assert_id_in_scope ident;
      "%" ^ ident.lexeme
    | Ast.Term Ast.Intlit intlit -> intlit.lexeme
    | Ast.Term Ast.Strlit strlit ->
       data_section := sprintf "%sdata %s = { b \"%s\", b 0 }\n"
                         !data_section (cons_tmpreg true) strlit.lexeme;
       !tmpreg
    | Ast.Term Ast.IntCompoundLit cl ->
        let _ = Err.err Err.Unimplemented __FILE__ __FUNCTION__
                  ~msg:"int compound literals are unimplemented" None in exit 1
    | Ast.Proc_call pc ->
       let args = List.fold_left (fun acc e ->
                      acc ^ "w " ^ evaluate_expr e ^ ", "
                    ) "" pc.args in
       if pc.id.lexeme = "printf" (* INTRINSIC *)
       then
         let cons_args = "call $printf(" ^ args ^ ")" in
         func_section := sprintf "%s    %s =w %s\n" !func_section (cons_tmpreg false) cons_args;
         !tmpreg
       else if pc.id.lexeme = "exit" (* INTRINSIC *)
       then
         let _ = assert (List.length pc.args = 1) in
         let cons_args = "call $exit(" ^ args ^ ")" in
         func_section := sprintf "%s    %s =w %s\n" !func_section (cons_tmpreg false) cons_args;
         !tmpreg
       else
         let _ = assert_id_in_scope pc.id in (* Temporary *)
         let cons_args = "call $" ^ pc.id.lexeme ^ "(" ^ args ^ ")" in

         let type_tok = snd (get_token_from_scope pc.id.lexeme) in
         let qbe_type = match type_tok with
           | None -> failwith "TODO ERR: NO TYPE"
           | Some t -> scoretype_to_qbetype t in

         (* Don't create a new REG if the proc call is void. *)
         (if qbe_type <> "" then
            func_section := sprintf "%s    %s =%s %s\n"
                              !func_section (cons_tmpreg false) qbe_type cons_args
          else
            func_section := sprintf "%s    %s\n"
                              !func_section cons_args);
         !tmpreg

  (* Evaluate the `return` statement. *)
  and evaluate_ret_stmt (stmt : Ast.ret_stmt) : unit =
    let expr = evaluate_expr stmt.expr in
    let type_tok = snd (get_token_from_scope !cur_proc_id) in
    let qbe_type = match type_tok with
      | None -> failwith "TODO ERR: NO TYPE"
      | Some t -> scoretype_to_qbetype t in
    func_section := sprintf "%s    ret %s\n" !func_section @@ if qbe_type = "" then "" else expr;
    didret := true

  (* Evaluate the `let` statement. *)
  and evaluate_let_stmt (stmt : Ast.let_stmt) : unit =
    assert_id_not_in_scope stmt.id;
    add_id_to_scope stmt.id.lexeme stmt.id @@ Some stmt.type_;
    let expr = evaluate_expr stmt.expr in
    func_section := sprintf "%s    %%%s =%s copy %s\n"
                      !func_section stmt.id.lexeme (scoretype_to_qbetype stmt.type_) expr

  (* Evaluate a block statement. *)
  and evaluate_block_stmt (stmt : Ast.block_stmt) : bool =
    push_scope ();

    let user_did_ret = ref false in
    (List.iter (fun s ->
         (match s with
          | Ast.Ret _ -> user_did_ret := true
          | _ -> user_did_ret := false);
         evaluate_stmt s
       ) stmt.stmts);

    (* List.iter evaluate_stmt stmt.stmts; *)
    pop_scope ();
    !user_did_ret

  (* Evaluate an `if` statement. *)
  and evaluate_if_stmt (stmt : Ast.if_stmt) : unit =
    let expr = evaluate_expr stmt.expr in
    let iflbl, elselbl, donelbl = cons_if_lbl () in
    func_section := sprintf "%s    jnz %s, %s, %s\n" !func_section expr iflbl
                      (if stmt.else_ = None then donelbl else elselbl);
    func_section := sprintf "%s%s\n" !func_section iflbl;
    let _ = evaluate_block_stmt stmt.block in

    (* Currently, if you have instructions after `ret` in QBE, it
     * fails. This is a QAD solution to this issue. *)
    if not !didret && not !didbreak then (* TODO: find a better solution *)
      func_section := sprintf "%s    jmp %s\n" !func_section donelbl;

    (match stmt.else_ with
     | Some block ->
        func_section := sprintf "%s%s\n" !func_section elselbl;
        let _ = evaluate_block_stmt block in ()
     | None -> ());
    func_section := sprintf "%s%s\n" !func_section donelbl

  (* Evaluate a statement expression ie `printf()`. *)
  and evaluate_expr_stmt (stmt : Ast.stmt_expr) : unit =
    let _ = evaluate_expr stmt in ()
    (* let expr = evaluate_expr stmt in () *)
    (* func_section := sprintf "%s    %s =w copy %s\n" !func_section (cons_tmpreg false) expr *)

  (* Evalute a `mut` statement ie `x = x + 1`. *)
  and evaluate_mut_stmt (stmt : Ast.mut_stmt) : unit =
    assert_id_in_scope stmt.id;
    let expr = evaluate_expr stmt.expr in
    func_section := sprintf "%s    %%%s =w copy %s\n" !func_section stmt.id.lexeme expr

  (* Evaluate a `while` statement. *)
  and evaluate_while_stmt (stmt : Ast.while_stmt) : unit =
    let looplbl, loopstartlbl, loopendlbl = cons_loop_lbl () in
    func_section := sprintf "%s%s\n" !func_section looplbl;
    let expr = evaluate_expr stmt.expr in
    func_section := sprintf "%s    jnz %s, %s, %s\n" !func_section expr loopstartlbl loopendlbl;
    func_section := sprintf "%s%s\n" !func_section loopstartlbl;
    let _ = evaluate_block_stmt stmt.block in

    (* Currently, if you have instructions after `ret` in QBE, it
     * fails. This is a QAD solution to this issue. *)
    if not !didret && not !didbreak then (* TODO: find a better solution *)
      func_section := sprintf "%s    jmp %s\n" !func_section looplbl;

    func_section := sprintf "%s%s\n" !func_section loopendlbl

  and evaluate_for_stmt (stmt : Ast.for_stmt) : unit =
    push_scope ();
    let looplbl, loopstartlbl, loopendlbl = cons_loop_lbl () in
    evaluate_stmt stmt.init;
    func_section := sprintf "%s%s\n" !func_section looplbl;
    let expr = evaluate_expr stmt.cond in
    func_section := sprintf "%s    jnz %s, %s, %s\n" !func_section expr loopstartlbl loopendlbl;
    func_section := sprintf "%s%s\n" !func_section loopstartlbl;
    let _ = evaluate_block_stmt stmt.block in
    evaluate_stmt stmt.after;

    (* Currently, if you have instructions after `ret` in QBE, it
     * fails. This is a QAD solution to this issue. *)
    if not !didret && not !didbreak then (* TODO: find a better solution *)
      func_section := sprintf "%s    jmp %s\n" !func_section looplbl;

    func_section := sprintf "%s%s\n" !func_section loopendlbl;
    pop_scope ()

  (* Evaluate a `break` statement. *)
  and evaluate_break_stmt (stmt : Token.t) : unit =
    printf "[WARN]: `break` statements are not fully functional\n";
    didbreak := true;
    func_section := sprintf "%s    jmp %s # BREAK\n" !func_section !loop_end_lbl

  (* Evaluate a statement and call the appropriate function. *)
  and evaluate_stmt (stmt : Ast.stmt) : unit =
    didret := false;
    didbreak := false;
    match stmt with
    | Ast.Proc_def procdef -> assert false
    | Ast.Block block ->
      let _ = Err.err Err.Unimplemented __FILE__ __FUNCTION__
                ~msg:"nested block statements are unimplemented" None in exit 1
    | Ast.Let letstmt -> evaluate_let_stmt letstmt
    | Ast.Mut mutstmt -> evaluate_mut_stmt mutstmt
    | Ast.If ifstmt -> evaluate_if_stmt ifstmt
    | Ast.While whilestmt -> evaluate_while_stmt whilestmt
    | Ast.Stmt_expr se -> evaluate_expr_stmt se
    | Ast.Ret ret -> evaluate_ret_stmt ret
    | Ast.Break b -> evaluate_break_stmt b
    | Ast.For f -> evaluate_for_stmt f

  (* Evaluate a procedure definition statement. *)
  and evaluate_proc_def_stmt (stmt : Ast.proc_def_stmt) : unit =
    cur_proc_id := stmt.id.lexeme;
    assert_id_not_in_scope stmt.id;
    add_id_to_scope stmt.id.lexeme stmt.id @@ Some stmt.rettype;

    push_scope ();

    let params : string =
      List.fold_left (fun acc p ->
          let id, type_ = (fst p).Token.lexeme, snd p in

          assert_id_not_in_scope (fst p);
          add_id_to_scope id (fst p) @@ Some type_;

          let qbe_type = scoretype_to_qbetype type_ in
          acc ^ qbe_type ^ " %" ^ id ^ ", "
        ) "" stmt.params in

    let type_tok = snd (get_token_from_scope stmt.id.lexeme) in
    let qbe_type = match type_tok with
      | None -> failwith "TODO ERR: NO TYPE"
      | Some t -> scoretype_to_qbetype t in

    func_section :=
      sprintf "%sexport function %s $%s(%s) {\n@start\n" !func_section qbe_type stmt.id.lexeme params;

    let user_did_ret = evaluate_block_stmt stmt.block in

    (if qbe_type = "" && not user_did_ret then
      func_section := sprintf "%s    ret\n" !func_section);

    func_section := sprintf "%s}\n" !func_section;
    pop_scope ()

  (* Evaluate a top-level statement. *)
  let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : unit =
    match stmt with
    | Ast.Proc_def s -> evaluate_proc_def_stmt s
    | Ast.Let s ->
      let _ = Err.err Err.Unimplemented __FILE__ __FUNCTION__
                ~msg:"global let statements are unimplemented" None in exit 1

  (* Entrypoint *)
  let generate_inter_lang (program : Ast.program) : string =
    List.iter evaluate_toplvl_stmt program;
    !func_section ^ !data_section

end
