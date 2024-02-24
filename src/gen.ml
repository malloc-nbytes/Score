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

   (* export function w $main() {
    @start
      call $dputs(l $prologue, w 1)
    @loop
      %r =w call $read(w 0, l $ch, w 1)
      %cmp =w ceqw %r, 1
      jnz %cmp, @maybeleft, @eof	# %cmp == 1 ? @maybeleft : @eof
    @maybeleft
      %b =w loadub $ch		# Bytes must be loaded as words
      %cmp =w ceqw %b, 60		# <
      jnz %cmp, @isleft, @mayberight
    @isleft
      call $dputs(l $left, w 1)
      jmp @loop
    @mayberight
      %cmp =w ceqw %b, 62		# >
      jnz %cmp, @isright, @maybedec
    @isright
      call $dputs(l $right, w 1)
      jmp @loop
    @maybedec
      %cmp =w ceqw %b, 45		# -
      jnz %cmp, @isdec, @maybeinc
    @isdec
      call $dputs(l $dec, w 1)
      jmp @loop
    @maybeinc
      %cmp =w ceqw %b, 43		# +
      jnz %cmp, @isinc, @maybegetchar
    @isinc
      call $dputs(l $inc, w 1)
      jmp @loop
    @maybegetchar
      %cmp =w ceqw %b, 44		# ,
      jnz %cmp, @isgetchar, @maybeputchar
    @isgetchar
      call $dputs(l $gchar, w 1)
      jmp @loop
    @maybeputchar
      %cmp =w ceqw %b, 46		# .
      jnz %cmp, @isputchar, @maybelopen
    @isputchar
      call $dputs(l $pchar, w 1)
      jmp @loop
    @maybelopen
      %cmp =w ceqw %b, 91		# [
      jnz %cmp, @islopen, @maybelclose
    @islopen
      call $islopen()
      jmp @loop
    @maybelclose
      %cmp =w ceqw %b, 93		# ]
      jnz %cmp, @islclose, @loop
    @islclose
      call $islclose()
      jmp @loop
    @eof
      %d =w loadsw $depth
      jnz %d, @mismatch, @done
    @mismatch
      call $mismatch()
      ret 0
    @done
      call $dputs(l $epilogue, w 1)
      ret 0
    } *)

module Gen = struct
  open Ast
  open Printf
  open Token

  let func_section = ref ""
  let data_section = ref ""

  let tmpreg = ref "%__SCORE_TMP_REG"
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

  let cons_tmpreg () : string =
    let tmp = !tmpreg_c in
    tmpreg_c := !tmpreg_c + 1;
    tmpreg := "%__SCORE_TMP_REG" ^ (string_of_int tmp);
    !tmpreg

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
      func_section := sprintf "%s    %s =w %s %s, %s\n" !func_section (cons_tmpreg ())
        (evaluate_binop bin.op) lhs rhs;
      !tmpreg
    | Ast.Term Ast.Ident ident -> "%" ^ ident.value
    | Ast.Term Ast.Intlit intlit -> intlit.value
    | Ast.Proc_call pc ->
      let args = (List.fold_left (fun acc e ->
        acc ^ "w " ^ evaluate_expr e ^ ", "
      ) "" pc.args) in
      let cons_args = "call $" ^ pc.id.value ^ "(" ^ args ^ ")" in
      func_section := sprintf "%s    %s =w %s\n" !func_section (cons_tmpreg ()) cons_args;
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

  and evaluate_stmt (stmt : Ast.stmt) : unit =
    didret := false;
    match stmt with
    | Ast.Proc_def procdef -> assert false
    | Ast.Block block -> assert false
    | Ast.Let letstmt -> evaluate_let_stmt letstmt
    | Ast.Mut mutstmt -> assert false
    | Ast.If ifstmt -> evaluate_if_stmt ifstmt
    | Ast.While whilestmt -> assert false
    | Ast.Stmt_expr se -> assert false
    | Ast.Ret ret -> evaluate_ret_stmt ret

  and evaluate_proc_def_stmt (stmt : Ast.proc_def_stmt) : unit =
    let params : string = List.fold_left (fun acc p ->
      let id = (fst p).Token.value in (* TODO: Types for params *)
      acc ^ "w %" ^ id ^ ", "
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
