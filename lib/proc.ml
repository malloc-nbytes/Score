module Proc = struct
  open Ast
  open Scope
  open Token

  (* TODO: only do this with export procs *)
  let evaluate_proc_def_stmt (pd : Ast.proc_def_stmt) : unit =
    if pd.export then
      let _ = Scope.assert_def_proc_not_in_tbl pd.id.lexeme in

      let params : (TokenType.id_type list) ref = ref [] in

      let _ = List.iter (fun param ->
          let param_type = (snd param) in
          params := !params @ [param_type]
        ) pd.params in

      Scope.def_proc_tbl_add pd.id.lexeme !params pd.rettype
    else
      ()

  let evaluate_import_stmt (stmt : Ast.import_stmt) : unit =
    Scope.state.imports <- stmt.path.lexeme :: Scope.state.imports

  let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : unit =
    match stmt with
    | Ast.Proc_def pd -> evaluate_proc_def_stmt pd
    | Ast.Import i ->
       if not (List.exists (fun n -> n = i.path.lexeme) Scope.state.compiled_files) then
         let _ = evaluate_import_stmt i in
         Scope.state.compiled_files <- i.path.lexeme :: Scope.state.compiled_files
    | Ast.Def_func df -> Scope.def_proc_tbl_add df.id.lexeme df.params df.rettype
    | _ -> ()

  let populate_proc_tbl (program : Ast.program) : string list =
    List.iter evaluate_toplvl_stmt program;
    let imports = Scope.state.imports in
    Scope.state.imports <- [];
    imports

end
