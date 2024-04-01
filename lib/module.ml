type t =
  { modname : string
  ; ast : Ast.program
  ; depends : t list
  ; exported_procs : Ast.proc_def_stmt
  ; exported_types : Ast.struct_stmt
  }

let iter_toplvl_stmts (stmts : Ast.toplvl_stmt list) (modname : string) (depends : string list) : t =
  ignore stmts;
  ignore modname;
  ignore depends;
  failwith "todo"

let rec gather_imports = function
  | [] -> []
  | (Ast.Import is) :: tl -> [is.path.lexeme] @ gather_imports tl
  | _ :: tl -> gather_imports tl

let produce_module (ast : Ast.program) : t =
  ignore iter_toplvl_stmts;
  ignore gather_imports;
  ignore ast;
  failwith "todo"
