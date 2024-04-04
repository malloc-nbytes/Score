type t =
  { modname : string
  ; ast : Ast.program
  ; depends : t list
  ; exported_procs : Ast.proc_def_stmt list
  ; exported_types : Ast.struct_stmt list
  }

val gather_imports : Ast.toplvl_stmt list -> string list
val produce_modules : (string, Ast.program) Hashtbl.t -> (string, string list) Hashtbl.t -> t list
