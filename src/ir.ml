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
  open Printf
  open Ast
  open Token
  open Err
  open Scope
  open Utils
  open Emit

  class label_maker =
    object (self)
      val reg = "__SCORE_REG"
      val mutable regc = 0

      val ret_lbl = "__RET_LBL"
      val mutable ret_lblc = 0

      val if_lbl = "__IF"
      val else_lbl = "__ELSE"
      val if_done_lbl = "__IF_DONE"
      val mutable if_lblc = 0

      val loop_entry_lbl = "__LOOP_ENTRY"
      val loop_begin_lbl = "__LOOP_BEGIN"
      val loop_end_lbl = "__LOOP_END"
      val mutable loop_lblc = 0

      method new_loop_lbl () : string * string * string =
        let c = string_of_int loop_lblc in
        let entry = loop_entry_lbl^c
        and begin_ = loop_begin_lbl^c
        and end_ = loop_end_lbl^c in
        loop_lblc <- loop_lblc+1;
        entry, begin_, end_

      method new_if_lbl () : string * string * string =
        let c = string_of_int if_lblc in
        let if_ = if_lbl^c
        and else_ = else_lbl^c
        and if_done = if_done_lbl^c in
        if_lblc <- if_lblc+1;
        if_, else_, if_done

      method new_ret_lbl () : string =
        let ret = ret_lbl ^ string_of_int ret_lblc in
        ret_lblc <- ret_lblc+1;
        ret

      method new_reg (global : bool) : string =
        let ret = (if global then "$" else "%") ^ reg ^ string_of_int regc in
        regc <- regc+1;
        ret
    end

  let lm = new label_maker

  let last_tok : (Token.t option) ref = ref None

  (* --- Helpers --- *)

  let types_compatable (type1 : TokenType.id_type) (type2 : TokenType.id_type) : bool =
    match type1, type2 with
    | a, b when a = b -> true
    (* I32 *)
    | TokenType.I32, TokenType.I32 -> true
    | TokenType.I32, TokenType.Number -> true
    | TokenType.Number, TokenType.I32 -> true
    (* Usize *)
    | TokenType.Usize, TokenType.Usize -> true
    | TokenType.Usize, TokenType.Number -> true
    | TokenType.Number, TokenType.Usize -> true
    (* Char *)
    | TokenType.Char, TokenType.I32 -> true
    | TokenType.Char, TokenType.Usize -> true
    | TokenType.Char, TokenType.Number -> true
    | TokenType.I32, TokenType.Char -> true
    | TokenType.Usize, TokenType.Char -> true
    | TokenType.Number, TokenType.Char -> true
    (* Structs *)
    | TokenType.Custom (_), TokenType.Array (_, _) -> true
    (* Other *)
    | TokenType.Pointer _, TokenType.Usize -> true
    (* Str *)
    (* | TokenType.Str, TokenType.Char -> true *)
    (* | TokenType.Char, TokenType.Str -> true *)
    (* Number *)
    | TokenType.Number, TokenType.Number -> true
    | _ -> false

  (* --- Evaluations --- *)

  let evaluate_binop = function
    | TokenType.Plus -> "add"
    | TokenType.Minus -> "sub"
    | TokenType.Asterisk -> "mul"
    | TokenType.ForwardSlash -> "div"
    | TokenType.DoubleEquals -> "ceqw"
    | TokenType.LessThan -> "csltw"
    | TokenType.GreaterThan -> "csgtw"
    | TokenType.LessThanEqual -> "cslew"
    | TokenType.GreaterThanEqual -> "csgew"
    | TokenType.NotEqual -> "cnew"
    | TokenType.Percent -> "rem"
    | TokenType.DoubleAmpersand -> "and"
    | TokenType.DoublePipe -> "or"
    | TokenType.PlusEquals -> "add"
    | TokenType.MinusEquals -> "sub"
    | TokenType.AsteriskEquals -> "mul"
    | TokenType.PercentEquals -> "rem"
    | _ -> failwith "evaluate_binop: invalid binop"

  let rec evaluate_expr (expr : Ast.expr) (force_long : bool) (callee_type : TokenType.id_type) : string * TokenType.id_type =
    match expr with
    | Ast.Binary bin ->
       let lhs, lhs_type = evaluate_expr bin.lhs force_long callee_type in
       let rhs, rhs_type = evaluate_expr bin.rhs force_long callee_type in
       let instr = evaluate_binop bin.op.ttype in
       let reg = lm#new_reg false in

       if types_compatable lhs_type rhs_type then
         (* NOTE: can use either lhs_type or rhs_type *)
         let _ = Emit.binop reg lhs_type lhs rhs instr in
         reg, lhs_type
       else
         let _ = Err.err_type_mismatch !last_tok lhs_type rhs_type in
         exit 1

    | Ast.Term Ast.Struct_access sa ->
       let id = sa.id.lexeme in

       let struct_name =
         match (Scope.get_token_from_scope id).type_ with
         | Custom name -> name
         | Pointer Custom name -> name
         | _ -> failwith "undeclared struct" in

       let member_id = sa.member.lexeme in
       let structure = Scope.get_struct_from_tbl struct_name in
       let member = List.find (fun (id, _, _) -> id.Token.lexeme = member_id) structure.members in
       let offset, type_ = match member with | _, t, offset -> offset, t in

       let reg = lm#new_reg false in
       Emit.binop reg TokenType.Usize ("%"^id) (string_of_int offset) "add";

       let reg2 = lm#new_reg false in
       Emit.load reg2 type_ reg;
       reg2, type_

    | Ast.Array_retrieval ar ->
       Scope.assert_token_in_scope ar.id;
       let stored_var = Scope.get_token_from_scope ar.id.lexeme in
       let stored_type = stored_var.type_ in
       let inner_type = Utils.unwrap_array stored_type in

       (match stored_type with
        | TokenType.Str | TokenType.Char ->
           let multiplicative = "1" in
           let index = Ast.Binary {lhs = ar.index;
                                   op = Token.{lexeme = "*"; ttype = TokenType.Asterisk; r=0; c=0; fp=""; macro=None};
                                   rhs = Ast.Term (Ast.Intlit (Token.{lexeme = multiplicative; ttype = TokenType.IntegerLiteral; r=0; c=0; fp=""; macro=None}))} in
           let index, index_type = evaluate_expr index true callee_type in
           let reg = lm#new_reg false in

           Emit.load reg TokenType.Usize ("%"^stored_var.id);
           Emit.binop index TokenType.Usize index reg "add";
           Emit.__instr reg TokenType.Usize index "loadsb";

           reg, inner_type

        | _ ->
           let multiplicative = Utils.scr_type_to_bytes inner_type in
           let index = Ast.Binary {lhs = ar.index;
                                   op = Token.{lexeme = "*"; ttype = TokenType.Asterisk; r=0; c=0; fp=""; macro=None};
                                   rhs = Ast.Term (Ast.Intlit (Token.{lexeme = multiplicative; ttype = TokenType.IntegerLiteral; r=0; c=0; fp=""; macro=None}))} in
           let index, index_type = evaluate_expr index true callee_type in
           let reg = lm#new_reg false in

           (if index_type <> TokenType.Usize && index_type <> TokenType.Number then
              let _ = Err.err_type_mismatch !last_tok TokenType.Usize index_type in
              exit 1);

           Emit.binop reg TokenType.Usize ("%"^ar.id.lexeme) index "add";

           let reg2 = lm#new_reg false in
           Emit.load reg2 inner_type reg;
           reg2, inner_type)

    | Ast.Term Ast.Ident ident ->
       Scope.assert_token_in_scope ident;
       let reg = lm#new_reg false in
       let stored_var = Scope.get_token_from_scope ident.lexeme in
       let stored_type = stored_var.type_ in
       last_tok := Some ident;

       (match stored_type with
        | TokenType.Array (_, _) ->
           (* We want to avoid loading with an array *)
           "%"^stored_var.id, TokenType.Usize
        | TokenType.Custom (_) ->
           "%"^stored_var.id, stored_var.type_
        | _ ->
           if stored_var.stack_allocd then Emit.load reg stored_type ("%" ^ ident.lexeme);
           reg, stored_type)

    | Ast.Term Ast.Intlit intlit ->
       intlit.lexeme, (if force_long then TokenType.Usize else TokenType.Number)

    | Ast.Dereference deref ->
       (match deref with
        | Ast.Term (Ast.Ident ident) ->
           Scope.assert_token_in_scope ident;
           let stored_var = Scope.get_token_from_scope ident.lexeme in
           let inner_type = Utils.unwrap_ptr stored_var.type_ in
           let reg = lm#new_reg false in
           let reg2 = lm#new_reg false in

           Emit.load reg TokenType.Usize ("%"^stored_var.id);
           Emit.load reg2 inner_type reg;
           reg2, inner_type
        | _ -> failwith "evaluate_expr: Ast.Dereference: unreachable")

    | Ast.Reference expr ->
       (match expr with
        | Ast.Term (Ast.Ident ident) ->
           Scope.assert_token_in_scope ident;
           let stored_var = Scope.get_token_from_scope ident.lexeme in
           let stored_type = stored_var.type_ in
           "%"^stored_var.id, TokenType.Pointer stored_type
        | _ -> failwith "evaluate_expr: Ast.Reference: unreachable")

    | Ast.Cast (cast_type, expr) ->
       let expr, expr_type = evaluate_expr expr force_long callee_type in
       if cast_type = expr_type then expr, cast_type
       else
         let reg = lm#new_reg false in
         (match cast_type, expr_type with
          | TokenType.I32, TokenType.Usize -> Emit.extsh reg cast_type expr; reg, cast_type
          | TokenType.Usize, TokenType.I32 -> Emit.extsw reg cast_type expr; reg, cast_type
          | _ -> failwith @@ sprintf "%s: cast error" __FUNCTION__)

    | Ast.Term Ast.Char chara -> string_of_int (Char.code (chara.lexeme.[0])), TokenType.Char

    | Ast.Term Ast.Strlit strlit ->
       let reg = lm#new_reg true in
       Emit.string_in_data_section reg strlit.lexeme;
       reg, TokenType.Str

    | Ast.Term (Ast.IntCompoundLit (exprs, len)) -> (* Stack allocated arrays *)
       let len = match len with | Some len -> len | None -> 0 in
       let array_reg = lm#new_reg false in
       let stride = if force_long then 8 else (int_of_string (Utils.scr_type_to_bytes callee_type)) in

       (match callee_type with
        | TokenType.Array ((TokenType.Custom struct_name), Some len) ->
           let structure = Scope.get_struct_from_tbl struct_name in
           let structure_members = structure.members in

           Emit.stack_alloc4 (String.sub array_reg 1 (String.length array_reg - 1)) (string_of_int (len*structure.size));
           for i = 0 to (List.length exprs) - 1 do
             let cur_member = List.nth structure_members i in
             let member_tok, member_type, member_offset = match cur_member with
               | t, ty, off -> t, ty, off in
             let added_reg = lm#new_reg false in
             Emit.binop added_reg TokenType.Usize array_reg (string_of_int (member_offset)) "add";
             let expr, expr_type = evaluate_expr (List.nth exprs i) force_long member_type in
             Emit.store expr added_reg member_type
           done;
           array_reg, TokenType.Array (TokenType.I32, Some len) (* TODO: fix this *)

        | TokenType.Custom struct_name -> (* dealing w/ a struct *)
           let structure = Scope.get_struct_from_tbl struct_name in
           let structure_members = structure.members in

           Emit.stack_alloc4 (String.sub array_reg 1 (String.length array_reg - 1)) (string_of_int (stride));

           for i = 0 to (List.length exprs) - 1 do
             let cur_member = List.nth structure_members i in
             let member_tok, member_type, member_offset = match cur_member with
               | t, ty, off -> t, ty, off in
             let added_reg = lm#new_reg false in
             Emit.binop added_reg TokenType.Usize array_reg (string_of_int (member_offset)) "add";
             let expr, expr_type = evaluate_expr (List.nth exprs i) force_long member_type in
             Emit.store expr added_reg member_type
           done;
           array_reg, TokenType.Array (TokenType.I32, Some len) (* TODO: fix this *)

        | _ -> (* dealing w/ an array *)
           Emit.stack_alloc4 (String.sub array_reg 1 (String.length array_reg - 1)) (string_of_int (len*stride));
           for i = 0 to (List.length exprs) - 1 do
             let added_reg = lm#new_reg false in
             Emit.binop added_reg TokenType.Usize array_reg (string_of_int (i*stride)) "add";
             let expr, expr_type = evaluate_expr (List.nth exprs i) force_long callee_type in
             Emit.store expr added_reg expr_type
           done;
           array_reg, TokenType.Array (TokenType.I32, Some len))

    | Ast.Proc_call pc ->
       (* TODO: verify proc params match *)
       let args = List.fold_left (fun acc e ->
                      let arg, arg_type = match evaluate_expr e force_long callee_type with
                        | a, TokenType.Custom (name) -> a, ":"^name
                        | a, at ->
                           let arg_type = Utils.scr_to_qbe_type at in
                           let arg_type = if arg_type = "b" then "w" else arg_type in
                           a, arg_type in
                      acc ^ arg_type ^ " " ^ arg ^ ", "
                    ) "" pc.args in
       (match pc.id.lexeme with
        | "printf" -> (* INTRINSIC *)
           let reg = lm#new_reg false in
           Emit.proc_call_wassign reg "printf" args TokenType.I32;
           reg, TokenType.I32
        | "strcmp" -> (* INTRINSIC *)
           assert (List.length pc.args = 2);
           let reg = lm#new_reg false in
           Emit.proc_call_wassign reg "strcmp" args TokenType.I32;
           reg, TokenType.I32
        | "malloc" -> (* INTRINSIC *)
           assert (List.length pc.args = 1);
           let reg = lm#new_reg false in
           Emit.proc_call_wassign reg "malloc" args TokenType.Usize;
           reg, TokenType.Usize
        | "free" -> (* INTRINSIC *)
           assert (List.length pc.args = 1);
           let reg = lm#new_reg false in
           Emit.proc_call_woassign "free" args;
           reg, TokenType.Usize
        | "exit" -> (* INTRINSIC *)
           assert (List.length pc.args = 1);
           Emit.proc_call_woassign "exit" args;
           "", TokenType.Void
        | _ -> (* user-defined proc *)
           let proc_rettype = ref TokenType.Void
           and reg = lm#new_reg false in

           (if Scope.check_def_proc_in_tbl pc.id.lexeme then
             let def_proc = Scope.get_def_proc_from_tbl pc.id.lexeme in
             proc_rettype := def_proc.rettype
           else
             let _ = Scope.assert_proc_in_tbl pc.id.lexeme in
             proc_rettype := Scope.get_proc_rettype_from_tbl pc.id.lexeme);

           if !proc_rettype = TokenType.Void then
             let _ = Emit.proc_call_woassign pc.id.lexeme args in
             reg, !proc_rettype (* NOTE: remove `reg` here if issues are caused *)
           else
             let _ = Emit.proc_call_wassign reg pc.id.lexeme args !proc_rettype in
             reg, !proc_rettype)

  let evaluate_let_stmt (stmt : Ast.let_stmt) : unit =
    let id = stmt.id
    and id_lexeme = stmt.id.lexeme
    and stmt_type = stmt.type_ in

    Scope.assert_token_not_in_scope id;

    let expr, expr_type = match stmt_type with
      | TokenType.Array (TokenType.Usize, _) -> evaluate_expr stmt.expr true TokenType.Usize
      | TokenType.Array (TokenType.Str, _) -> evaluate_expr stmt.expr true TokenType.Str
      | TokenType.Array (TokenType.Char, _) -> evaluate_expr stmt.expr false TokenType.Char
      | TokenType.Array (TokenType.I32, _) -> evaluate_expr stmt.expr false TokenType.I32
      | TokenType.Array (TokenType.Custom (name), len) ->
         evaluate_expr stmt.expr true (TokenType.Array (TokenType.Custom (name), len))
      | (TokenType.Custom (struct_name)) as structure_type ->
         evaluate_expr stmt.expr false structure_type
      | TokenType.Array (_,_) -> failwith "evaluate_let_stmt: unimplemented"
      | _ -> evaluate_expr stmt.expr false stmt_type in

    let bytes = Utils.scr_type_to_bytes stmt_type in

    match stmt_type with
    | TokenType.Array (inner_type, len) ->
       Emit.copy ("%" ^ id_lexeme) stmt_type expr;
       Scope.add_id_to_scope id_lexeme id stmt_type true
    | TokenType.Custom (_) ->
       Emit.copy ("%" ^ id_lexeme) stmt_type expr;
       Scope.add_id_to_scope id_lexeme id stmt_type true
    | _ ->
       if types_compatable stmt_type expr_type then
         let _ = Scope.add_id_to_scope id_lexeme id stmt_type true in
         let _ = Emit.stack_alloc4 id_lexeme bytes in
         Emit.store expr ("%" ^ id_lexeme) stmt_type
       else
         let _ = Err.err_type_mismatch (Some id) stmt_type expr_type in
         exit 1

  let rec evaluate_block_stmt (bs : Ast.block_stmt) : unit =
    Scope.push ();
    List.iter evaluate_stmt bs.stmts;
    Scope.pop ()

  and evaluate_proc_def_stmt (pd : Ast.proc_def_stmt) : unit =
    Scope.state.cur_proc_id <- pd.id.lexeme, pd.rettype;
    Scope.assert_id_not_in_scope pd.id.lexeme;
    Scope.add_proc_to_tbl pd;

    Scope.push ();
    (* Make sure params are not in scope.
     * Add the params to the scope. *)
    List.iter (fun param ->
        let id = (fst param)
        and param_type = (snd param)
        and id_lexeme = (fst param).Token.lexeme in
        Scope.assert_token_not_in_scope id;
        Scope.add_id_to_scope id_lexeme id param_type true
      ) pd.params;

    Emit.proc_def pd.export pd.id.lexeme pd.params pd.rettype;

    (* stack alloc params *)
    List.iter (fun param ->
        let id_lexeme = (fst param).Token.lexeme
        and param_type = (snd param) in
        let bytes = Utils.scr_type_to_bytes param_type in

        match param_type with
        | TokenType.Array (_, _) -> ()
        | TokenType.Custom (_) -> ()
        | TokenType.Pointer TokenType.Custom (_) -> ()
        | _ ->
           let reg = lm#new_reg false in
           Emit.copy reg param_type ("%" ^ id_lexeme);
           Emit.stack_alloc4 id_lexeme bytes;
           Emit.store reg ("%" ^ id_lexeme) param_type
      ) pd.params;

    evaluate_block_stmt pd.block;
    Scope.pop ();

    (if pd.rettype = TokenType.Void then
       let ret_lbl = lm#new_ret_lbl () in
       Emit.ret "" ret_lbl);

    Emit.rbrace ()

  and evaluate_ret_stmt (stmt : Ast.ret_stmt) : unit =
    match stmt.expr with
    | Some expr ->
       let expr, expr_type = evaluate_expr (Utils.unwrap stmt.expr) false TokenType.Void in
       ignore expr_type; (* TODO: make sure this matches cur_proc rettype *)
       Emit.ret expr (lm#new_ret_lbl ())
    | None -> Emit.ret "" (lm#new_ret_lbl ())

  and evaluate_stmt_expr (stmt : Ast.stmt_expr) : unit =
    let expr, type_ = evaluate_expr stmt false TokenType.Void in
    ignore expr;
    ignore type_

  and evaluate_mut_stmt (stmt : Ast.mut_stmt) : unit =
    match stmt.left with
    | Ast.Term Ast.Ident ident ->
       Scope.assert_token_in_scope ident;
       let stored_var = Scope.get_token_from_scope ident.lexeme in
       let expr, expr_type = evaluate_expr stmt.right false stored_var.type_ in
       let mut_type = stored_var.type_ in
       let mut_id = ident in

       assert (stored_var.stack_allocd);

       if types_compatable mut_type expr_type then Emit.store expr ("%" ^ mut_id.lexeme) mut_type
       else
         let _ = Err.err_type_mismatch (Some mut_id) mut_type expr_type in
         exit 1

    | Ast.Dereference deref ->
       let left, left_type = match deref with
         | Ast.Term (Ast.Ident ident) ->
            Scope.assert_token_in_scope ident;
            let stored_var = Scope.get_token_from_scope ident.lexeme in
            let stored_type = stored_var.type_ in
            let inner_type = Utils.unwrap_ptr stored_type in

            assert (stored_var.stack_allocd);

            let reg = lm#new_reg false in
            Emit.load reg TokenType.Usize ("%" ^ ident.lexeme);
            reg, inner_type
         | _ -> failwith "evaluate_mut_stmt: Ast.Dereference: unreachable" in

       let right, right_type = evaluate_expr stmt.right false left_type in
       if types_compatable left_type right_type then Emit.store right left left_type
       else failwith @@ sprintf "%s: type mismatch: %s <> %s" __FUNCTION__
                          (TokenType.id_type_to_string left_type)
                          (TokenType.id_type_to_string right_type)
    | Ast.Array_retrieval ar ->
       Scope.assert_token_in_scope ar.id;

       let stored_var = Scope.get_token_from_scope ar.id.lexeme in
       let stored_type = stored_var.type_ in
       let inner_type = Utils.unwrap_array stored_type in

       (match stored_type with
        | TokenType.Str ->
           let multiplicative = "1" in
           let index = Ast.Binary {lhs = ar.index;
                                   op = Token.{lexeme = "*"; ttype = TokenType.Asterisk; r=0; c=0; fp=""; macro=None};
                                   rhs = Ast.Term (Ast.Intlit (Token.{lexeme = multiplicative; ttype = TokenType.IntegerLiteral; r=0; c=0; fp=""; macro=None}))} in
           let index, index_type = evaluate_expr index true stored_type in
           let reg = lm#new_reg false in

           Emit.load reg TokenType.Usize ("%"^stored_var.id);
           Emit.binop index TokenType.Usize index reg "add";
           let expr, _ = evaluate_expr stmt.right false stored_type in
           Emit.store expr index TokenType.Char
        | _ ->
           let multiplicative = Utils.scr_type_to_bytes inner_type in
           let index = Ast.Binary {lhs = ar.index;
                                   op = Token.{lexeme = "*"; ttype = TokenType.Asterisk; r=0; c=0; fp=""; macro=None};
                                   rhs = Ast.Term (Ast.Intlit (Token.{lexeme = multiplicative; ttype = TokenType.IntegerLiteral; r=0; c=0; fp=""; macro=None}))} in
           let index, index_type = evaluate_expr index true stored_type in
           let expr, expr_type = evaluate_expr stmt.right false stored_type in
           let array_reg = "%"^ar.id.lexeme in
           let added_reg = lm#new_reg false in
           Emit.binop added_reg TokenType.Usize array_reg index "add";
           Emit.store expr added_reg expr_type)
    | Ast.Term Ast.Struct_access sa ->
        let id = sa.id.lexeme in

        let struct_name =
          match (Scope.get_token_from_scope id).type_ with
          | Custom name -> name
          | Pointer Custom name -> name
          | _ -> failwith "undeclared struct" in

        let member_id = sa.member.lexeme in
        let structure = Scope.get_struct_from_tbl struct_name in
        let member = List.find (fun (id, _, _) -> id.Token.lexeme = member_id) structure.members in
        let offset, type_ = match member with | _, t, offset -> offset, t in

        let reg = lm#new_reg false in
        Emit.binop reg TokenType.Usize ("%"^id) (string_of_int offset) "add";

        let expr, expr_type = evaluate_expr stmt.right false type_ in
        Emit.store expr reg expr_type
    | _ -> failwith "evaluate_mut_stmt: unimplemented mut_stmt"

  and evaluate_if_stmt (stmt : Ast.if_stmt) : unit =
    let condition, condition_type = evaluate_expr stmt.expr false TokenType.Void in
    let if_lbl, else_lbl, if_done_lbl = lm#new_if_lbl () in
    Emit.jnz condition if_lbl (if stmt.else_ = None then if_done_lbl else else_lbl);
    Emit.lbl if_lbl;

    evaluate_block_stmt stmt.block;

    (match List.hd (List.rev stmt.block.stmts) with
     | Ast.Ret _ -> ()
     | _ -> Emit.jmp if_done_lbl);

    (match stmt.else_ with
     | Some block ->
        Emit.lbl else_lbl;
        evaluate_block_stmt block
     | None -> ());

    Emit.lbl if_done_lbl

  and evaluate_while_stmt (stmt : Ast.while_stmt) : unit =
    let loop_entry_lbl, loop_begin_lbl, loop_end_lbl = lm#new_loop_lbl () in
    Emit.lbl loop_entry_lbl;

    let cond, _ = evaluate_expr stmt.expr false TokenType.Void in

    Emit.jnz cond loop_begin_lbl loop_end_lbl;
    Emit.lbl loop_begin_lbl;

    evaluate_block_stmt stmt.block;

    (match List.hd (List.rev stmt.block.stmts) with
     | Ast.Ret _ -> ()
     | _ -> Emit.jmp loop_entry_lbl);

    Emit.lbl loop_end_lbl

  and evaluate_for_stmt (stmt : Ast.for_stmt) : unit =
    Scope.push ();

    let loop_entry_lbl, loop_begin_lbl, loop_end_lbl = lm#new_loop_lbl () in
    evaluate_stmt stmt.init;

    Emit.lbl loop_entry_lbl;

    let expr, _ = evaluate_expr stmt.cond false TokenType.Void in

    Emit.jnz expr loop_begin_lbl loop_end_lbl;
    Emit.lbl loop_begin_lbl;
    evaluate_block_stmt stmt.block;
    evaluate_stmt stmt.after;

    (match List.hd (List.rev stmt.block.stmts) with
     | Ast.Ret _ -> ()
     | _ -> Emit.jmp loop_entry_lbl);

    Emit.lbl loop_end_lbl;
    Scope.pop ()

  and evaluate_struct_stmt (stmt : Ast.struct_stmt) : unit =
    (* TODO: assert id not in scope *)

    let id = stmt.id.lexeme in

    let fields = stmt.fields in

    let qbe_types = List.fold_left (fun acc f ->
                        let type_ = Utils.scr_to_qbe_type (snd f) in
                        let type_ = if type_ = "b" then "w" else type_ in
                        acc ^ type_ ^ ", "
                      ) "" fields in

    let rec find_size lst =
      match lst with
      | [] -> 0
      | hd :: tl -> int_of_string (Utils.scr_type_to_bytes (snd hd)) + find_size tl in

    Scope.add_struct_to_tbl id fields (find_size fields);

    Emit.new_type id qbe_types

  and evaluate_stmt = function
    | Ast.Proc_def stmt -> failwith "evaluate_stmt: Ast.Proc_def unimplemented"
    | Ast.Block stmt -> failwith "evaluate_stmt: Ast.Block unimplemented"
    | Ast.Let stmt -> evaluate_let_stmt stmt
    | Ast.Mut stmt -> evaluate_mut_stmt stmt
    | Ast.If stmt -> evaluate_if_stmt stmt
    | Ast.While stmt -> evaluate_while_stmt stmt
    | Ast.Stmt_expr stmt -> evaluate_stmt_expr stmt
    | Ast.Ret stmt -> evaluate_ret_stmt stmt
    | Ast.Break stmt -> failwith "evaluate_stmt: Ast.Break unimplemented"
    | Ast.For stmt -> evaluate_for_stmt stmt

  let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : unit =
    match stmt with
    | Ast.Proc_def pd -> evaluate_proc_def_stmt pd
    | Ast.Import i -> ()
    | Ast.Struct s -> evaluate_struct_stmt s
    | Ast.Let l -> evaluate_let_stmt l
    | Ast.Def_func df -> Scope.def_proc_tbl_add df.id.lexeme df.params df.rettype

  (* --- Entrypoint --- *)
  let generate_ir (program : Ast.program) : string =
    List.iter evaluate_toplvl_stmt program;
    let ret = Scope.state.type_section ^ Scope.state.func_section ^ Scope.state.data_section in
    Scope.state.func_section <- "";
    Scope.state.data_section <- "";
    Scope.state.type_section <- "";
    ret

end

