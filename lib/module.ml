type t =
  { modname : string
  ; ast : Ast.program
  ; depends : t list
  ; exported_procs : Ast.proc_def_stmt list
  ; exported_types : Ast.struct_stmt list
  }

let rec gather_imports = function
  | [] -> []
  | (Ast.Import is) :: tl -> [is.path.lexeme] @ gather_imports tl
  | _ :: tl -> gather_imports tl

let produce_module (ast : Ast.program) : t =
  let modname = ref ""
  and depends = ref []
  and exported_procs = ref []
  and exported_types = ref [] in

  let iter_toplvl_stmts (stmts : Ast.toplvl_stmt list) : unit =
    ignore stmts;
    failwith "todo"
  in

  iter_toplvl_stmts ast;
  {modname = !modname; ast; depends = !depends; exported_procs = !exported_procs; exported_types = !exported_types }
