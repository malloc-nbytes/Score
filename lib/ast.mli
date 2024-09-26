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

open Token

module Ast : sig

  type program = toplvl_stmt list

  and toplvl_stmt =
    | Proc_Def of stmt_proc
    | Let of stmt_let
    | Module of stmt_module
    | Import of stmt_import
    | Struct of stmt_struct
    | Def of stmt_def
    | Extern of stmt_extern

  and stmt =
    | Let of stmt_let
    | Proc of stmt_proc
    | Block of stmt_block
    | Stmt_Expr of expr
    | Return of expr
    | For of stmt_for
    | If of stmt_if
    | Mut of stmt_mut
    | While of stmt_while

  and stmt_struct =
    { id : Token.t
    ; members : (Token.t * TokenType.id_type) list
    ; export : bool
    }

  and stmt_while =
    { expr : expr
    ; block : stmt_block
    }

  and stmt_mut =
    { left : expr
    ; op : Token.t
    ; right : expr
    }

  and stmt_for =
    { start : stmt
    ; _while : expr
    ; _end : stmt
    ; block : stmt_block
    }

  and stmt_if =
    { expr : expr
    ; block : stmt_block
    ; _else : stmt_block option
    }

  and stmt_module =
    { id : Token.t
    }

  and stmt_import =
    { filepath : Token.t
    }

  and stmt_let =
    { id : Token.t
    ; ty : TokenType.id_type
    ; expr : expr
    ; export : bool
    }

  and stmt_block = stmt list

  and stmt_proc =
    { id : Token.t
    ; params : (Token.t * TokenType.id_type) list
    ; rettype : TokenType.id_type
    ; block : stmt_block
    ; export : bool
    ; variadic : bool
    }

  and stmt_def =
    { id : Token.t
    ; params : (Token.t * TokenType.id_type) list
    ; rettype : TokenType.id_type
    ; export : bool
    ; variadic : bool
    }

  and stmt_extern =
    { id : Token.t
    ; params : (Token.t * TokenType.id_type) list
    ; rettype : TokenType.id_type
    ; export : bool
    ; variadic : bool
    }

  and expr =
    | Term of expr_term
    | Binary of expr_binary
    | Unary of expr_unary

  and expr_term =
    | IntLit of Token.t
    | StrLit of Token.t
    | CharLit of Token.t
    | BoolLit of bool
    | Ident of Token.t
    | Proc_Call of expr_proc_call
    | Index of expr_index

  and expr_index =
    { accessor : expr
    ; idx : expr
    }

  and expr_binary =
    { lhs : expr
    ; op : Token.t
    ; rhs : expr
    }

  and expr_unary =
    { op : Token.t
    ; expr : expr
    }

  and expr_proc_call =
    { lhs : Token.t
    ; args : expr list
    }
end
