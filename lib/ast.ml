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

module Ast = struct

  type program = toplvl_stmt list

  and toplvl_stmt =
    | Proc_Def of stmt_proc
    | Let of stmt_let
    | Module of stmt_module
    | Import of stmt_import

  and stmt =
    | Let of stmt_let
    | Proc of stmt_proc
    | Block of stmt_block
    | Stmt_Expr of expr
    | Return of expr
    | For of stmt_for
    | If of stmt_if
    | Mut of stmt_mut

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
    }

  and expr =
    | Term of expr_term
    | Binary of expr_binary
    | Unary of expr_unary

  and expr_term =
    | IntLit of Token.t
    | StrLit of Token.t
    | CharLit of Token.t
    | Ident of Token.t
    | Proc_Call of expr_proc_call

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
    { lhs : expr
    ; args : expr list
    }

  let rec spaces = function
    | 0 -> ()
    | k -> let _ = Printf.printf " " in spaces (k-1)

  let rec debug_print_expr expr s start_spaces newline =
    let open Printf in
    let open Token in

    let debug_print_expr_term term =
      let debug_print_expr_term_proc_call (proc_call : expr_proc_call) (s : int) : unit =
        let aux (proc_call : expr_proc_call) (s : int) : unit =
          printf "src=";
          debug_print_expr proc_call.lhs 0 false true;
          List.iter (fun expr ->
              debug_print_expr expr (s+2) true true
            ) proc_call.args
        in

        printf "PROC_CALL(";
        aux proc_call (s+2);
        spaces (s+2);
        printf ")"
      in

      printf "TERM(";
      let _ = match term with
        | IntLit k -> printf "INTLIT(%s)" k.lexeme
        | StrLit k -> printf "STRLIT(%s)" k.lexeme
        | CharLit k -> printf "CHARLIT(%s)" k.lexeme
        | Ident k -> printf "IDENT(%s)" k.lexeme
        | Proc_Call k -> debug_print_expr_term_proc_call k s
      in
      printf ")"
    in

    let debug_print_expr_binary binary s =
      printf "BINARY(\n";
      debug_print_expr binary.lhs (s+2) true true;
      spaces (s+2);
      printf "op=%s\n" binary.op.lexeme;
      debug_print_expr binary.rhs (s+2) true true;
      spaces s;
      printf ")"
    in

    (if start_spaces then spaces s
    else ());
    printf "EXPR(";
    let _ = match expr with
      | Term term -> debug_print_expr_term term;
      | Binary binary -> debug_print_expr_binary binary s;
      | Unary _ -> failwith ""
    in
    if newline then printf ")\n"
    else printf ")"

  let rec debug_print_stmt_proc (proc : stmt_proc) s =
    let open Printf in
    let open Token in
    let open TokenType in
    spaces s;
    printf "PROC_DEF(export=%b, id=%s, args=" proc.export proc.id.lexeme;
    List.iter (fun param -> printf "%s:%s," (fst param).lexeme (string_of_id_type (snd param))) proc.params;
    printf ")";
    printf ": %s {\n" @@ string_of_id_type proc.rettype;
    List.iter (fun st -> debug_print_stmt st (s+2)) proc.block;
    printf "}\n"

  and debug_print_stmt_let (_let : stmt_let) s =
    let open Printf in
    let open Token in
    spaces s;
    printf "LET(export=%b, id=%s) = " _let.export _let.id.lexeme;
    debug_print_expr _let.expr (s+2) false true

  and debug_print_stmt_module _module =
    let open Printf in
    printf "MODULE %s\n" _module.id.lexeme

  and debug_print_stmt_import _import =
    let open Printf in
    printf "IMPORT %s\n" _import.filepath.lexeme

  and debug_print_stmt_return return s =
    let open Printf in
    spaces s;
    printf "RETURN ";
    debug_print_expr return s false true

  and debug_print_stmt_if _if s =
    let open Printf in
    spaces s;
    printf "IF(expr="; debug_print_expr _if.expr s false true;
    spaces s;
    printf "BLOCK={\n";
    spaces s;
    List.iter (fun st -> debug_print_stmt st s) _if.block;
    spaces (s);
    printf "}\n";
    match _if._else with
    | Some _else -> List.iter (fun st -> debug_print_stmt st s) _if.block;
    | None -> ()

  and debug_print_stmt_for (_for : stmt_for) s =
    let open Printf in
    spaces s;
    printf "FOR(start="; debug_print_stmt _for.start 0;
    spaces (s+2);
    printf "; while="; debug_print_expr _for._while (s+4) false true;
    spaces (s+2);
    printf "; after=\n"; debug_print_stmt _for._end (s+2);
    spaces (s+2);
    printf "block={\n";
    List.iter (fun st -> debug_print_stmt st s) _for.block;
    spaces (s+2);
    printf "}\n";
    ()

  and debug_print_stmt_mut mut s =
    let open Printf in
    let open Token in
    spaces s;
    printf "MUT(left=";
    debug_print_expr mut.left s false false;
    printf ", op=`%s`" mut.op.lexeme;
    printf ", right=";
    debug_print_expr mut.right s false false;
    printf ")\n";

  and debug_print_stmt stmt s =
    spaces s;
    match stmt with
    | Let _let -> debug_print_stmt_let _let s
    | Proc proc -> debug_print_stmt_proc proc s
    | Block block -> List.iter (fun st -> debug_print_stmt st s) block
    | Stmt_Expr se -> debug_print_expr se s true true
    | Return return -> debug_print_stmt_return return s
    | If _if -> debug_print_stmt_if _if s
    | For _for -> debug_print_stmt_for _for s
    | Mut mut -> debug_print_stmt_mut mut s

  let debug_print_program program =
    Printf.printf "--- DUMPING GENERATED AST ---\n";
    let rec aux (stmt : toplvl_stmt list) : unit =
      match stmt with
      | [] -> ()
      | (Proc_Def pd) :: tl ->
        debug_print_stmt_proc pd 0;
        aux tl
      | (Let _let) :: tl ->
        debug_print_stmt_let _let 0;
        aux tl
      | (Module _module) :: tl ->
        debug_print_stmt_module _module;
        aux tl
      | (Import _import) :: tl ->
        debug_print_stmt_import _import;
        aux tl in
    aux program;
    Printf.printf "--- DONE ---\n";

end
