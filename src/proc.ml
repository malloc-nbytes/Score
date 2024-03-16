module Proc = struct
  open Ast
  open Scope
  open Token

  let evaluate_proc_def_stmt (pd : Ast.proc_def_stmt) : unit =
    Scope.assert_id_not_in_scope pd.id.lexeme;
    Scope.add_proc_to_tbl pd;

    List.iter (fun param ->
        let id = (fst param)
        and param_type = (snd param)
        and id_lexeme = (fst param).Token.lexeme in
        ignore id;
        ignore param_type;
        ignore id_lexeme;
        ()
      ) pd.params;
    ()

  let evaluate_import_stmt (stmt : Ast.import_stmt) : unit =
    Scope.state.imports <- stmt.path.lexeme :: Scope.state.imports

  let evaluate_toplvl_stmt (stmt : Ast.toplvl_stmt) : unit =
    match stmt with
    | Ast.Proc_def pd -> evaluate_proc_def_stmt pd
    | Ast.Import i -> evaluate_import_stmt i
    | Ast.Def_func df -> Scope.def_proc_tbl_add df.id.lexeme df.params df.rettype
    | _ -> ()

  let populate_proc_tbl (program : Ast.program) : string list =
    List.iter evaluate_toplvl_stmt program;
    Scope.state.imports

end
