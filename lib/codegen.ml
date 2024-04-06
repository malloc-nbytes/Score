module Codegen = struct
  open Token

  let scorety_to_llvmty (scr_ty : TokenType.id_type) (ctx : Llvm.llcontext) =
    match scr_ty with
    | TokenType.I32 -> Llvm.i32_type ctx
    | _ -> failwith "todo"

  module SM = Map.Make(String)

  let compile_procedure (stmt : Ast.proc_def_stmt) ctx md : unit =
    ignore ctx;
    ignore md;
    ignore stmt;

    let i32 = scorety_to_llvmty stmt.rettype ctx in
    let fty = Llvm.function_type i32 (Array.init (List.length stmt.params) (Fun.const i32)) in
    let fv = Llvm.define_function stmt.id.lexeme fty md in

    (* let env =
      List.fold_left () (0, SM.empty) 
    in *)

    ignore fty;
    ignore fv;
    failwith "todo"

  let codegen (module_ : Module.t) : unit =
    let ctx = Llvm.create_context () in
    let md = Llvm.create_module ctx module_.modname in

    ignore ctx;
    ignore md;
    ignore module_;
    ignore compile_procedure;

    failwith "todo"

end

