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
module Gen = struct
  open Ast
  open Printf
  open Token

  (* Main program. A list of functions. *)
  let func_section = ref ""

  (* Data section. A list of different globals. *)
  let data_section = ref ""

  (* Variable to use when a computation takes multiple steps. *)
  let tmpreg = ref "%__SCORE_TMP_REG"

  (* Variable to use to make `tmpreg` unqiue. *)
  let tmpreg_c = ref 0

  let if_lbl = ref "@if"
  let else_lbl = ref "@else"
  let done_lbl = ref "@done"
  let if_c = ref 0
  let didret = ref false (* TODO: Find a better solution *)

  let cons_if_lbl () : string * string * string =
    let tmp = string_of_int !if_c in
    if_c := !if_c + 1;

    if_lbl := "@if" ^ tmp;
    else_lbl := "@else" ^ tmp;
    done_lbl := "@done" ^ tmp;

    !if_lbl, !else_lbl, !done_lbl

  let cons_tmpreg (global : bool) : string =
    let tmp = !tmpreg_c in
    tmpreg_c := !tmpreg_c + 1;
    tmpreg := (if global then "$" else "%") ^ "__SCORE_TMP_REG" ^ (string_of_int tmp);
    !tmpreg

  let scoretype_to_qbetype (token : Token.t) : string =
    match token.Token.value with
    | "i32" -> "w"
    | "str" -> "l"
    | _ -> failwith @@ sprintf "gen.ml: invalid type `%s`" token.Token.value

  let evaluate_binop (op : Token.t) : string =
    match op.ttype with
    | TokenType.Plus -> "add"
    | TokenType.Minus -> "sub"
    | TokenType.Asterisk -> "mul"
    | TokenType.ForwardSlash -> "div"
    | TokenType.DoubleEquals -> "ceqw"
    | _ -> failwith @@ sprintf "Invalid binary operator %s" op.value

  let rec evaluate_expr (expr : Ast.expr) : string =
    match expr with
    | Ast.Binary bin ->
       let lhs = evaluate_expr bin.lhs in
       let rhs = evaluate_expr bin.rhs in
       func_section := sprintf "%s    %s =w %s %s, %s\n" !func_section (cons_tmpreg false)
                         (evaluate_binop bin.op) lhs rhs;
       !tmpreg
    | Ast.Term Ast.Ident ident -> "%" ^ ident.value
    | Ast.Term Ast.Intlit intlit -> intlit.value
    | Ast.Term Ast.Strlit strlit ->
       data_section := sprintf "%sdata %s = { b \"%s\", b 0 }\n"
                         !data_section (cons_tmpreg true) strlit.value;
       !tmpreg
    | Ast.Proc_call pc ->
       let args = (List.fold_left (fun acc e ->
                       acc ^ "w " ^ evaluate_expr e ^ ", "
                     ) "" pc.args) in
       if pc.id.value = "puts"
       then
         let _ = assert (List.length pc.args = 1) in
         let cons_args = "call $puts(" ^ args ^ ")" in
         func_section := sprintf "%s    %s =w %s\n" !func_section (cons_tmpreg false) cons_args;
         !tmpreg
       else
         let cons_args = "call $" ^ pc.id.value ^ "(" ^ args ^ ")" in
         func_section := sprintf "%s    %s =w %s\n" !func_section (cons_tmpreg false) cons_args;
         !tmpreg

  and evaluate_ret_stmt (stmt : Ast.ret_stmt) : unit =
    let expr = evaluate_expr stmt.expr in
    func_section := sprintf "%s    ret %s\n" !func_section expr;
    didret := true

  and evaluate_mut_stmt (stmt : Ast.mut_stmt) : unit =
    assert false

  and evaluate_let_stmt (stmt : Ast.let_stmt) : unit =
    let expr = evaluate_expr stmt.expr in
    func_section := sprintf "%s    %%%s =w copy %s\n" !func_section stmt.id.value expr

  and evaluate_block_stmt (stmt : Ast.block_stmt) : unit =
    List.iter evaluate_stmt stmt.stmts

  and evaluate_if_stmt (stmt : Ast.if_stmt) : unit =
    let expr = evaluate_expr stmt.expr in
    let iflbl, elselbl, donelbl = cons_if_lbl () in
    func_section := sprintf "%s    jnz %s, %s, %s\n" !func_section expr iflbl (if stmt.else_ = None then donelbl else elselbl);
    func_section := sprintf "%s%s\n" !func_section iflbl;
    evaluate_block_stmt stmt.block;

    (* Currently, if you have instructions after `ret` in QBE, it
     * fails. This is a QAD solution to this issue. *)
    if not !didret then
      func_section := sprintf "%s    jmp %s\n" !func_section donelbl;

    (match stmt.else_ with
     | Some block ->
        func_section := sprintf "%s%s\n" !func_section elselbl;
        evaluate_block_stmt block
     | None -> ());
    func_section := sprintf "%s%s\n" !func_section donelbl

  and evaluate_expr_stmt (stmt : Ast.stmt_expr) : unit =
    let expr = evaluate_expr stmt in
    func_section := sprintf "%s    %s =w copy %s\n" !func_section (cons_tmpreg false) expr

  and evaluate_stmt (stmt : Ast.stmt) : unit =
    didret := false;
    match stmt with
    | Ast.Proc_def procdef -> assert false
    | Ast.Block block -> assert false
    | Ast.Let letstmt -> evaluate_let_stmt letstmt
    | Ast.Mut mutstmt -> assert false
    | Ast.If ifstmt -> evaluate_if_stmt ifstmt
    | Ast.While whilestmt -> assert false
    | Ast.Stmt_expr se -> evaluate_expr_stmt se
    | Ast.Ret ret -> evaluate_ret_stmt ret

  and evaluate_proc_def_stmt (stmt : Ast.proc_def_stmt) : unit =
    let params : string =
      List.fold_left (fun acc p ->
          let id, type_ = (fst p).Token.value, snd p in
          let qbe_type = scoretype_to_qbetype type_ in
          let id = (fst p).Token.value in (* TODO: Types for params *)
          acc ^ qbe_type ^ " %" ^ id ^ ", "
        ) "" stmt.params in
    func_section :=
      sprintf "%sexport function w $%s(%s) {\n@start\n" !func_section stmt.id.value params;
    evaluate_block_stmt stmt.block;
    func_section := sprintf "%s}\n" !func_section

  let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : unit =
    match stmt with
    | Ast.Proc_def s -> evaluate_proc_def_stmt s
    | Ast.Let s -> evaluate_let_stmt s

  let generate_inter_lang (program : Ast.program) : string =
    List.iter evaluate_toplvl_stmt program;
    !func_section ^ !data_section

end
