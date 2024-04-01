type t =
  { modname : string
  ; ast : Ast.program
  ; depends : t list
  ; exported_procs : Ast.proc_def_stmt
  ; exported_types : Ast.struct_stmt
  }

val gather_imports : Ast.toplvl_stmt list -> string list
val produce_module : Ast.program -> t
