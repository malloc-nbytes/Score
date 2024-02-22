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

module Ast = struct
  open Token

  type program = toplvl_stmt list

  and toplvl_stmt =
    | Proc_def of proc_def_stmt
    | Let of let_stmt

  and stmt =
    | Proc_def of proc_def_stmt
    | Block of block_stmt
    | Let of let_stmt
    | Mut of mut_stmt
    | If of if_stmt
    | While of while_stmt
    | Stmt_expr of stmt_expr

  and block_stmt = { stmts : stmt list}

  and while_stmt =
    { expr : expr
    ; block : block_stmt
    }

  and if_stmt =
    { expr : expr
    ; block : block_stmt
    }

  and proc_def_stmt =
    { id : Token.t
    ; params : (Token.t * TokenType.t) list
    ; block : block_stmt
    ; rettype : TokenType.t
    }

  and let_stmt =
    { id : Token.t
    ; type_ : Token.t
    ; expr : expr
    }

  and mut_stmt =
    { id : Token.t
    ; expr : expr
    }

  and expr =
    | Binary of binary_expr
    | Term of term_expr
    | Proc_call of proc_call_expr

  and stmt_expr =
    | Proc_call of proc_call_expr

  and binary_expr =
    { lhs : expr
    ; rhs : expr
    ; op : Token.t
    }

  and term_expr =
    | Ident of Token.t
    | Intlit of Token.t

  and proc_call_expr =
    { id : Token.t
    ; args : expr list
    }

  let ast_dump (program : program) : unit =
    let open Printf in

    let indent (d : int) : string =
      let s = ref "" in
      let _ = for i = 1 to d do s := !s ^ "  "
              done in !s in

    let rec expr_dump (expr : expr) (depth : int) : unit =
      let spaces = indent depth in
      match expr with
      | Binary b ->
         let _ = expr_dump b.lhs (depth + 1) in
         let _ = printf "%s%s\n" spaces b.op.value in
         expr_dump b.rhs (depth + 1)
      | Term Ident i -> printf "%s%s\n" spaces i.value
      | Term Intlit t -> printf "%s%s\n" spaces t.value
      | Proc_call pc -> failwith "Proc_call dump unimplemented" in

    let mut_stmt_dump (stmt : mut_stmt) (depth : int) : unit =
      let spaces = indent depth in
      let _ = printf "%sMUT %s =\n" spaces stmt.id.value in
      expr_dump stmt.expr depth in

    let let_stmt_dump (stmt : let_stmt) (depth : int) : unit =
      let spaces = indent depth in
      let _ = printf "%sLET %s =\n" spaces stmt.id.value in
      expr_dump stmt.expr depth in

    let rec proc_def_stmt_dump (stmt : proc_def_stmt) (depth : int) : unit =
      let spaces = indent depth in
      let _ = printf "%sPROC %s(" spaces stmt.id.value in
      let _ = List.iter(fun param ->
                  let pval = (fst param).Token.value
                  and ptype = (snd param) |> TokenType.to_string in
                  printf "%s %s," pval ptype) stmt.params in
      let _ = printf "): %s {\n" @@ TokenType.to_string stmt.rettype in
      let _ = block_stmt_dump stmt.block (depth + 1) in
      printf "}\n"

    and if_stmt_dump (stmt : if_stmt) (depth : int) : unit =
      let spaces = indent depth in
      let _ = printf "%sIF\n" spaces in
      let _ = expr_dump stmt.expr depth in
      let _ = printf "%s{\n" spaces in
      let _ = block_stmt_dump stmt.block (depth + 1) in
      printf "%s}\n" spaces

    and while_stmt_dump (stmt : while_stmt) (depth : int) : unit =
      let spaces = indent depth in
      let _ = printf "%sWHILE\n" spaces in
      let _ = expr_dump stmt.expr depth in
      let _ = printf "%s{\n" spaces in
      let _ = block_stmt_dump stmt.block (depth + 1) in
      printf "%s}\n" spaces

    and stmt_dump (stmt : stmt) (depth : int) : unit =
      match stmt with
      | Proc_def pd -> proc_def_stmt_dump pd (depth + 1)
      | Block b -> block_stmt_dump b (depth + 1)
      | Let l -> let_stmt_dump l (depth + 1)
      | Mut m -> mut_stmt_dump m (depth + 1)
      | If i -> if_stmt_dump i (depth + 1)
      | While w -> while_stmt_dump w (depth + 1)
      | Stmt_expr se -> failwith "Stmt_expr dump unimplemented"

    and block_stmt_dump (stmt : block_stmt) (depth : int) : unit =
      List.iter (fun s -> stmt_dump s (depth + 1)) stmt.stmts in

    let toplvl_stmt_dump (stmt : toplvl_stmt) : unit =
      match stmt with
      | Proc_def s -> proc_def_stmt_dump s 0
      | Let s -> let_stmt_dump s 0 in

    List.iter toplvl_stmt_dump program

end
