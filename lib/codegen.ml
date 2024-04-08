module Codegen = struct
  open Token

  (* let scorety_to_llvmty (type_: TokenType.id_type) (ctx : Llvm.llcontext) = *)
  (*     match type_ with *)
  (*     | TokenType.I32 -> Llvm.i32_type ctx *)
  (*     | _ -> failwith "todo" *)

  let rec compile_expr expr ctx md nv builder : Llvm.llvalue =
    match expr with
    | Ast.Term Ast.Intlit i -> Llvm.const_int (Llvm.i32_type ctx) (int_of_string i.lexeme)
    | Ast.Term Ast.Ident ident -> Hashtbl.find nv ident.lexeme
    | Ast.Proc_call pce ->
       let callee : Llvm.llvalue = match Llvm.lookup_function pce.id.lexeme md with
         | Some k -> k
         | None -> failwith @@ Printf.sprintf "compile_expr: procedure %s is not in scope" pce.id.lexeme in
       (if Array.length (Llvm.params callee) <> List.length (pce.args) then
          failwith @@ Printf.sprintf "compile_expr: number of params past to procedure %s is incorrect" pce.id.lexeme);
       let args = List.map (fun a -> compile_expr a ctx md nv builder) pce.args in
       Llvm.build_call (Llvm.i32_type ctx) callee (Array.of_list args) "calltmp" builder (* TODO: change i32_type *)
    | Ast.Binary be ->
       let lhs = compile_expr be.lhs ctx md nv builder
       and rhs = compile_expr be.rhs ctx md nv builder in
       (match be.op with
        | {ttype = TokenType.Plus; _} -> Llvm.build_add lhs rhs "addtmp" builder
        | {ttype = TokenType.Minus; _} -> Llvm.build_sub lhs rhs "subtmp" builder
        | {ttype = TokenType.Asterisk; _} -> Llvm.build_mul lhs rhs "multmp" builder
        | {ttype = TokenType.ForwardSlash; _} -> Llvm.build_udiv lhs rhs "divtmp" builder
        | {ttype = TokenType.GreaterThan; _} -> Llvm.build_icmp Llvm.Icmp.Sgt lhs rhs "gttmp" builder
        | {ttype = TokenType.LessThan; _} -> Llvm.build_icmp Llvm.Icmp.Slt lhs rhs "lttmp" builder
        | _ -> failwith @@ Printf.sprintf "invalid binop: %s" be.op.lexeme)
    | _ -> failwith "todo"

  (* let compile_procedure (stmt : Ast.proc_def_stmt) ctx md builder nv : unit = *)
  (*   ignore nv; *)
  (*   ignore ctx; *)
  (*   ignore md; *)
  (*   ignore stmt; *)
  (*   ignore builder; *)

  (*   let i32 = scorety_to_llvmty stmt.rettype ctx in *)
  (*   let func_ty = Llvm.function_type i32 (Array.init (List.length stmt.params) (Fun.const i32)) in *)
  (*   let func_def = Llvm.define_function stmt.id.lexeme func_ty md in *)

  (*   ignore func_ty; *)
  (*   ignore func_def; *)
  (*   ignore compile_expr; *)
  (*   failwith "todo" *)

  let codegen (module_ : Module.t) : unit =
    let the_context = Llvm.create_context () in
    let the_module = Llvm.create_module the_context module_.modname in
    let builder = Llvm.builder the_context in
    let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 20 in

    ignore the_context;
    ignore named_values;
    ignore the_module;
    ignore module_;
    ignore builder;
    ignore compile_expr;

    failwith "todo"

end

