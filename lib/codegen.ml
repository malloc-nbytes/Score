open Token

type context =
  { ctx : Llvm.llcontext
  ; md : Llvm.llmodule
  ; builder : Llvm.llbuilder
  ; nv : (string, Llvm.llvalue) Hashtbl.t
  }

let unwrap k = match k with | Some k -> k | None -> failwith "unwrapped None value"

let scorety_to_llvmty (type_: TokenType.id_type) (context : context) =
  match type_ with
  | TokenType.I32 -> Llvm.i32_type context.ctx
  | TokenType.Usize -> Llvm.i64_type context.ctx
  | _ -> failwith @@
           Printf.sprintf "scorety_to_llvmty: unimplemented type: %s"
             (TokenType.string_of_id_type type_)

let rec compile_expr (expr : Ast.expr) (context : context) : Llvm.llvalue =
  match expr with
  | Ast.Term Ast.Intlit i -> Llvm.const_int (Llvm.i32_type context.ctx) (int_of_string i.lexeme)
  | Ast.Term Ast.Ident ident -> Hashtbl.find context.nv ident.lexeme
  | Ast.Proc_call pce ->
     let callee : Llvm.llvalue = match Llvm.lookup_function pce.id.lexeme context.md with
       | Some k -> k
       | None -> failwith @@ Printf.sprintf "compile_expr: procedure %s is not in scope" pce.id.lexeme in
     (if Array.length (Llvm.params callee) <> List.length (pce.args) then
        failwith @@ Printf.sprintf "compile_expr: number of params past to procedure %s is incorrect" pce.id.lexeme);
     let args = List.map (fun a -> compile_expr a context) pce.args in
     (* TODO: change i32_type *)
     Llvm.build_call (Llvm.i32_type context.ctx) callee (Array.of_list args) "calltmp" context.builder
  | Ast.Binary be ->
     let lhs = compile_expr be.lhs context
     and rhs = compile_expr be.rhs context in
     (match be.op with
      | {ttype = TokenType.Plus; _} -> Llvm.build_add lhs rhs "addtmp" context.builder
      | {ttype = TokenType.Minus; _} -> Llvm.build_sub lhs rhs "subtmp" context.builder
      | {ttype = TokenType.Asterisk; _} -> Llvm.build_mul lhs rhs "multmp" context.builder
      | {ttype = TokenType.ForwardSlash; _} -> Llvm.build_udiv lhs rhs "divtmp" context.builder
      | {ttype = TokenType.GreaterThan; _} -> Llvm.build_icmp Llvm.Icmp.Sgt lhs rhs "gttmp" context.builder
      | {ttype = TokenType.LessThan; _} -> Llvm.build_icmp Llvm.Icmp.Slt lhs rhs "lttmp" context.builder
      | _ -> failwith @@ Printf.sprintf "invalid binop: %s" be.op.lexeme)
  | _ -> failwith "compile_expr: todo"

and compile_block_stmt (s : Ast.block_stmt) (context : context) : context * Llvm.llvalue =
  let rec aux lst ctx v =
    match lst with
    | [] -> ctx, (match v with | Some v -> v | None -> failwith "function does not return")
    | hd :: tl -> let ctx, v = compile_stmt hd context in aux tl ctx (Some v) in
  aux s.stmts context None

and compile_procedure (stmt : Ast.proc_def_stmt) (context : context) : context * Llvm.llvalue =
  let proc_rettype = scorety_to_llvmty stmt.rettype context in
  let ptypes = List.map (fun t -> scorety_to_llvmty t context) (List.map snd stmt.params) in
  let proc_ty = Llvm.function_type proc_rettype (Array.of_list ptypes) in
  let proc_def = Llvm.define_function stmt.id.lexeme proc_ty context.md in
  ignore proc_def;

  let bb : Llvm.llbasicblock = Llvm.append_block context.ctx "entry" proc_def in
  Llvm.position_at_end bb context.builder;

  (* TODO: use context.nv *)
  (* Hashtbl.clear context.nv; *)
  let context, value = compile_block_stmt stmt.block context in

  context, value

and compile_ret_stmt (s : Ast.ret_stmt) (context : context) : Llvm.llvalue =
  let value = compile_expr (unwrap s.expr) context in
  Llvm.build_ret value context.builder

and compile_let_stmt (s : Ast.let_stmt) (context : context) : context * Llvm.llvalue =
  ignore s;
  ignore context;
  failwith "todo"

and compile_stmt (s : Ast.stmt) (context : context) : context * Llvm.llvalue =
  match s with
  | Ast.Proc_def _ -> failwith "compile_stmt proc_def: todo"
  | Ast.Block _ -> failwith "compile_stmt block: todo"
  | Ast.Let l ->
     let context, value = compile_let_stmt l context in
     context, value
  | Ast.Mut _ -> failwith "compile_stmt mut: todo"
  | Ast.If _ -> failwith "compile_stmt if: todo"
  | Ast.While _ -> failwith "compile_stmt while: todo"
  | Ast.Stmt_expr _ -> failwith "compile_stmt stmt_expr: todo"
  | Ast.Ret r -> context, (compile_ret_stmt r context)
  | Ast.Break _ -> failwith "compile_stmt break: todo"
  | Ast.For _ -> failwith "compile_stmt for: todo"

let rec compile_toplvl_stmts (stmts : Ast.toplvl_stmt list) (context : context) : context =
  match stmts with
  | [] -> context
  | hd :: tl ->
     let context', _ = match hd with
       | Ast.Proc_def pd -> compile_procedure pd context
       | _ -> failwith "compile_toplvl_stmts: unreachable" in
     compile_toplvl_stmts tl context'

let compile_program (module_ : Module.t) : Llvm.llmodule =
  let ctx = Llvm.create_context () in
  let md = Llvm.create_module ctx module_.modname in
  let builder = Llvm.builder ctx in
  let nv : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 20 in

  let context = {ctx; md; builder; nv} in

  let context = compile_toplvl_stmts module_.ast context in

  ignore compile_expr;
  ignore compile_stmt;

  context.md

