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

and evaluate_stmt = function
  | Ast.Proc_def _ -> failwith "todo"
  | Ast.Block _ -> failwith "todo"
  | Ast.Let _ -> failwith "todo"
  | Ast.Mut _ -> failwith "todo"
  | Ast.If _ -> failwith "todo"
  | Ast.While _ -> failwith "todo"
  | Ast.Stmt_expr _ -> failwith "todo"
  | Ast.Ret _ -> failwith "todo"
  | Ast.Break _ -> failwith "todo"
  | Ast.For _ -> failwith "todo"

let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : unit =
  match stmt with
  | Ast.Proc_def _ -> failwith "todo"
  | Ast.Import _ -> failwith "todo"
  | Ast.Struct _ -> failwith "todo"
  | Ast.Let _ -> failwith "todo"
  | Ast.Def_func _ -> failwith "todo"
  | Ast.Module _ -> failwith "todo"

(* --- Entrypoint --- *)
let generate_ir (program : Ast.program) : string =
  ignore program;
  ignore evaluate_binop;
  ignore evaluate_stmt;
  ignore evaluate_toplvl_stmt;
  ""


