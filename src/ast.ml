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
    | Ret of ret_stmt
    | Break of Token.t
    | For of for_stmt

  and block_stmt = { stmts : stmt list }

  and ret_stmt = { expr : expr }

  and while_stmt =
    { expr : expr
    ; block : block_stmt
    }

  and for_stmt =
    { init : stmt
    ; cond : expr
    ; after : stmt
    ; block : block_stmt
    }

  and if_stmt =
    { expr : expr
    ; block : block_stmt
    ; else_ : block_stmt option
    }

  and proc_def_stmt =
    { id : Token.t
    ; params : (Token.t * TokenType.id_type) list
    ; block : block_stmt
    ; rettype : TokenType.id_type
    }

  and let_stmt =
    { id : Token.t
    ; type_ : TokenType.id_type
    ; expr : expr
    }

  and mut_stmt =
    | Mut_var of mut_var_stmt
    | Mut_arr of mut_arr_stmt

  and mut_var_stmt =
    { id : Token.t
    ; expr : expr
    }

  and mut_arr_stmt =
    { id : Token.t
    ; index : expr
    ; expr : expr
    }

  and expr =
    | Binary of binary_expr
    | Term of term_expr
    | Proc_call of proc_call_expr
    | Array_retrieval of array_retrieval_expr

  and array_retrieval_expr =
    { id : Token.t
    ; index : expr
    }

  and stmt_expr = expr

  and binary_expr =
    { lhs : expr
    ; rhs : expr
    ; op : Token.t
    }

  and term_expr =
    | Ident of Token.t
    | Intlit of Token.t
    | Strlit of Token.t
    | IntCompoundLit of expr list * (int option)

  and proc_call_expr =
    { id : Token.t
    ; args : expr list
    }

end
