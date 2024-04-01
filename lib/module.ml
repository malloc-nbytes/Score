type t =
  { modname : string
  ; ast : Ast.program
  ; depends : t list
  ; exported_procs : Ast.proc_def_stmt
  ; exported_types : Ast.struct_stmt
  }

let iter_toplvl_stmts (stmts : Ast.toplvl_stmt list) (modname : string) (depends : string list) () : t =
  match stmts with
  | 

let produce_module (ast : Ast.program) : t =
  let modname = "" in
  let depends = [] in
  let exported_procs = [] in
  let exported_types = [] in
  


