open Token

type variable =
  { tok : Token.t
  ; type_ : TokenType.id_type
  }

let g_ctx : Llvm.llcontext = Llvm.global_context ()
let g_md : Llvm.llmodule = Llvm.create_module g_ctx "global module"
let g_builder : Llvm.llbuilder = Llvm.builder g_ctx
let g_nv : (((variable, Llvm.llvalue) Hashtbl.t) list) ref = ref (Hashtbl.create 20 :: [])

let nvpush () = g_nv := (Hashtbl.create 20) :: !g_nv

let nvpop () =
  match !g_nv with
  | [] -> failwith "scope_pop: empty scope"
  | _ :: tl -> g_nv := tl

let addvar_to_nv (var : variable) (value : Llvm.llvalue) : unit =
  match !g_nv with
  | [] -> failwith "addvar_to_nv: empty scope"
  | hd :: _ -> Hashtbl.add hd var value

let findvar_in_nv (var : variable) : Llvm.llvalue =
  let rec aux lst =
    match lst with
    | [] -> failwith "findvar_in_nv: variable not found"
    | hd :: tl -> (match Hashtbl.find_opt hd var with
                   | Some v -> v
                   | None -> aux tl) in
  aux !g_nv

let unwrap k = match k with | Some k -> k | None -> failwith "unwrapped None value"

let scorety_to_llvmty (type_: TokenType.id_type) : Llvm.lltype =
  match type_ with
  | TokenType.I32 -> Llvm.i32_type g_ctx
  | TokenType.Usize -> Llvm.i64_type g_ctx
  | _ -> failwith @@
           Printf.sprintf "scorety_to_llvmty: unimplemented type: %s"
             (TokenType.string_of_id_type type_)

let emit_entry_alloca (fn : Llvm.llvalue) (var_name : string) (var_ty : TokenType.id_type) : Llvm.llvalue =
  let builder = Llvm.builder_at g_ctx (Llvm.instr_begin (Llvm.entry_block fn)) in
  Llvm.build_alloca (scorety_to_llvmty var_ty) var_name builder

let rec compile_expr (expr : Ast.expr) : Llvm.llvalue =
  match expr with
  | Ast.Term Ast.Intlit i -> Llvm.const_int (Llvm.i32_type g_ctx) (int_of_string i.lexeme)
  | Ast.Term Ast.Ident ident -> ignore ident; failwith "Todo"
  | Ast.Proc_call pce ->
     let callee : Llvm.llvalue = match Llvm.lookup_function pce.id.lexeme g_md with
       | Some k -> k
       | None -> failwith @@ Printf.sprintf "compile_expr: procedure %s is not in scope" pce.id.lexeme in
     (if Array.length (Llvm.params callee) <> List.length (pce.args) then
        failwith @@ Printf.sprintf "compile_expr: number of params past to procedure %s is incorrect" pce.id.lexeme);
     let args = List.map (fun a -> compile_expr a) pce.args in
     (* TODO: change i32_type *)
     Llvm.build_call (Llvm.i32_type g_ctx) callee (Array.of_list args) "calltmp" g_builder
  | Ast.Binary be ->
     let lhs = compile_expr be.lhs
     and rhs = compile_expr be.rhs in
     (match be.op with
      | {ttype = TokenType.Plus; _} -> Llvm.build_add lhs rhs "addtmp" g_builder
      | {ttype = TokenType.Minus; _} -> Llvm.build_sub lhs rhs "subtmp" g_builder
      | {ttype = TokenType.Asterisk; _} -> Llvm.build_mul lhs rhs "multmp" g_builder
      | {ttype = TokenType.ForwardSlash; _} -> Llvm.build_udiv lhs rhs "divtmp" g_builder
      | {ttype = TokenType.GreaterThan; _} -> Llvm.build_icmp Llvm.Icmp.Sgt lhs rhs "gttmp" g_builder
      | {ttype = TokenType.LessThan; _} -> Llvm.build_icmp Llvm.Icmp.Slt lhs rhs "lttmp" g_builder
      | _ -> failwith @@ Printf.sprintf "invalid binop: %s" be.op.lexeme)
  | _ -> failwith "compile_expr: todo"

and compile_block_stmt (s : Ast.block_stmt) : Llvm.llvalue =
  let rec aux lst v =
    match lst with
    | [] -> (match v with | Some v -> v | None -> failwith "function does not return")
    | hd :: tl -> let v = compile_stmt hd in aux tl (Some v) in
  aux s.stmts None

and compile_procedure_def (stmt : Ast.proc_def_stmt) : Llvm.llvalue =
  let proc_rettype = scorety_to_llvmty stmt.rettype in
  let ptypes = List.map (fun t -> scorety_to_llvmty t) (List.map snd stmt.params) in

  let proc_ty = Llvm.function_type proc_rettype (Array.of_list ptypes) in
  let proc_def = Llvm.define_function stmt.id.lexeme proc_ty g_md in
  let bb : Llvm.llbasicblock = Llvm.append_block g_ctx "entry" proc_def in
  Llvm.position_at_end bb g_builder;

  nvpush ();
  List.iter2 (fun ((param : Token.t), (param_type : TokenType.id_type)) (llvm_param : Llvm.llvalue) ->
      let alloca = emit_entry_alloca proc_def param.lexeme param_type in
      let _ = Llvm.build_store llvm_param alloca g_builder in
      addvar_to_nv {tok = param; type_ = param_type} alloca
    ) stmt.params (Array.to_list (Llvm.params proc_def));

  let ret = compile_block_stmt stmt.block in
  nvpop ();

  ret

and compile_ret_stmt (s : Ast.ret_stmt) : Llvm.llvalue =
  let value = compile_expr (unwrap s.expr) in
  Llvm.build_ret value g_builder

and compile_let_stmt (s : Ast.let_stmt) : Llvm.llvalue =
  ignore s;
  failwith "compile_let_stmt: unimplemented"
  (* let ty = scorety_to_llvmty s.type_ in *)
  (* let value = compile_expr s.expr in *)
  (* let alloca = Llvm.build_alloca ty s.id.lexeme g_builder in *)
  (* let _ = Llvm.build_store value alloca g_builder in *)
  (* Hashtbl.add nv s.id.lexeme alloca; *)
  (* alloca *)

and compile_stmt (s : Ast.stmt) : Llvm.llvalue =
  match s with
  | Ast.Proc_def _ -> failwith "compile_stmt proc_def: todo"
  | Ast.Block _ -> failwith "compile_stmt block: todo"
  | Ast.Let l -> compile_let_stmt l
  | Ast.Mut _ -> failwith "compile_stmt mut: todo"
  | Ast.If _ -> failwith "compile_stmt if: todo"
  | Ast.While _ -> failwith "compile_stmt while: todo"
  | Ast.Stmt_expr _ -> failwith "compile_stmt stmt_expr: todo"
  | Ast.Ret r -> compile_ret_stmt r
  | Ast.Break _ -> failwith "compile_stmt break: todo"
  | Ast.For _ -> failwith "compile_stmt for: todo"

let rec compile_toplvl_stmts (stmts : Ast.toplvl_stmt list) : unit =
  match stmts with
  | [] -> ()
  | hd :: tl ->
     let _ = match hd with
       | Ast.Proc_def pd -> compile_procedure_def pd
       | _ -> failwith "compile_toplvl_stmts: unimplemented toplvl statement" in
     compile_toplvl_stmts tl

let compile_program (module_ : Module.t) : Llvm.llmodule =
  ignore emit_entry_alloca;
  ignore g_nv;
  ignore nvpush;
  ignore nvpop;
  ignore addvar_to_nv;
  ignore findvar_in_nv;

  compile_toplvl_stmts module_.ast;
  g_md