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

module Emit = struct
  open Printf
  open Token
  open Utils
  open Scope

  let proc_def
        (export : bool)
        (procname : string)
        (params : (Token.t * TokenType.id_type) list)
        (rettype : TokenType.id_type) : unit =

    let emitted_procname = procname
    and emitted_params = List.fold_left (fun acc param ->
                             let id = (fst param).Token.lexeme in
                             match snd param with
                             | TokenType.Custom struct_name ->
                                let structure = Scope.get_struct_from_tbl struct_name in
                                acc ^ ":" ^ structure.Scope.id ^ " %" ^ id ^ ", "
                             | _ ->
                                (* let type_ = Utils.scr_to_qbe_type @@ snd param in *)
                                (* let type_ = if type_ = "b" then "w" else type_ in *)
                                acc ^ "l" ^ " %" ^ id ^ ", "
                           ) "" params

    and emitted_export = if export then "export" else ""
    and emitted_rettype = Utils.scr_to_qbe_type rettype in
    let emitted_rettype = if emitted_rettype = "b" then "w" else emitted_rettype in

    Scope.state.func_section <-
      sprintf "%s%s function %s $%s(%s) {\n@start\n"
        Scope.state.func_section emitted_export emitted_rettype emitted_procname emitted_params

  let string_in_data_section (id : string) (strlit : string) : unit =
    Scope.state.data_section <-
      sprintf "%sdata %s = { b \"%s\", b 0 }\n"
        Scope.state.data_section id strlit

  let stack_alloc4 (id : string) (bytes : string) : unit =
    let emitted_id = id in
    Scope.state.func_section <-
      sprintf "%s    %%%s =l alloc4 %s\n" Scope.state.func_section emitted_id bytes

  let stack_alloc8 (id : string) (bytes : string) : unit =
    let emitted_id = id in
    Scope.state.func_section <-
      sprintf "%s    %%%s =l alloc8 %s\n" Scope.state.func_section emitted_id bytes

  let stack_alloc16 (id : string) (bytes : string) : unit =
    let emitted_id = id in
    Scope.state.func_section <-
      sprintf "%s    %%%s =l alloc16 %s\n" Scope.state.func_section emitted_id bytes

  let store (value : string) (loc : string) (type_ : TokenType.id_type) : unit =
    let emitted_value = value
    and emitted_location = loc
    and emitted_type = Utils.scr_to_qbe_type type_ in
    Scope.state.func_section <-
      sprintf "%s    store%s %s, %s\n" Scope.state.func_section  emitted_type emitted_value emitted_location

  let load (id : string) (type_ : TokenType.id_type) (loc : string) : unit =
    let emitted_id = id
    and emitted_type = Utils.scr_to_qbe_type type_ in

    let emitted_type = if emitted_type = "b" then "w" else emitted_type in
    let emitted_type' = match type_ with | TokenType.Char -> "sb" | _ -> emitted_type
    and emitted_loc = loc in

    Scope.state.func_section <-
      sprintf "%s    %s =%s load%s %s\n"
        Scope.state.func_section emitted_id emitted_type emitted_type' emitted_loc

  let __instr (id : string) (type_ : TokenType.id_type) (rhs : string) (instr : string) : unit =
    let emitted_id = id
    and emitted_type = Utils.scr_to_qbe_type type_ in
    let emitted_type = if emitted_type = "b" then "w" else emitted_type in
    Scope.state.func_section <-
      sprintf "%s    %s =%s %s %s\n" Scope.state.func_section emitted_id emitted_type instr rhs

  let rbrace () : unit =
    Scope.state.func_section <- sprintf "%s}\n" Scope.state.func_section

  let copy (id : string) (type_ : TokenType.id_type) (rhs : string) : unit =
    __instr id type_ rhs "copy"

  let extsb (id : string) (type_ : TokenType.id_type) (rhs : string) : unit =
    __instr id type_ rhs "extsb"

  let extsw (id : string) (type_ : TokenType.id_type) (rhs : string) : unit =
    __instr id type_ rhs "extsw"

  let extsh (id : string) (type_ : TokenType.id_type) (rhs : string) : unit =
    __instr id type_ rhs "extsh"

  let assignment (lhs : string) (type_ : TokenType.id_type) (rhs : string) : unit =
    let emitted_type = Utils.scr_to_qbe_type type_ in
    Scope.state.func_section <- sprintf "%s    %s =%s %s\n" Scope.state.func_section lhs emitted_type rhs

  let binop (id : string) (type_ : TokenType.id_type) (left : string) (right : string) (binop : string) : unit =
    let emitted_id = id
    and emitted_type = Utils.scr_to_qbe_type type_
    and emitted_left = left
    and emitted_right = right
    and emitted_binop = binop in
    let emitted_type = if emitted_type = "b" then "w" else emitted_type in
    Scope.state.func_section <-
      sprintf "%s    %s =%s %s %s, %s\n"
        Scope.state.func_section emitted_id emitted_type emitted_binop emitted_left emitted_right

  let ret (value : string) (lbl : string) : unit =
    Scope.state.func_section <- sprintf "%s@%s\n" Scope.state.func_section lbl;
    Scope.state.func_section <- sprintf "%s    ret %s\n" Scope.state.func_section value

  let proc_call_wassign (assignee : string) (name : string) (args : string) (rettype : TokenType.id_type) : unit =
    let emitted_type = Utils.scr_to_qbe_type rettype in
    let emitted_type = if emitted_type = "b" then "w" else emitted_type in
    Scope.state.func_section <-
      sprintf "%s    %s =%s call $%s(%s)\n" Scope.state.func_section assignee emitted_type name args

  let proc_call_woassign (name : string) (args : string) : unit =
    Scope.state.func_section <-
      sprintf "%s    call $%s(%s)\n" Scope.state.func_section name args

  let jnz (cond : string) (true_lbl : string) (false_lbl : string) : unit =
    Scope.state.func_section <-
      sprintf "%s    jnz %s, @%s, @%s\n"
        Scope.state.func_section cond true_lbl false_lbl

  let lbl (lbl : string) : unit =
    Scope.state.func_section <- sprintf "%s@%s\n" Scope.state.func_section lbl

  let jmp (lbl : string) : unit =
    Scope.state.func_section <- sprintf "%s    jmp @%s\n" Scope.state.func_section lbl

  let new_type (type_name : string) (types : string) : unit =
    Scope.state.type_section <-
      sprintf "%stype :%s = { %s }\n" Scope.state.type_section type_name types

end
