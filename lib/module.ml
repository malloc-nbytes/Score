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

let produce_modules (ast : Ast.program list) (import_deps : (string, string list) Hashtbl.t) : t list =
  let rec iter_toplvl_stmts (stmts : Ast.toplvl_stmt list) : unit =
    match stmts with
    | [] -> ()
    | Ast.Proc_def _ :: _ -> failwith "Proc_def: todo"
    | Ast.Struct _ :: _ -> failwith "Struct: todo"
    | Ast.Import _ :: _ -> failwith "Import: todo"
    | Ast.Module _ :: _ -> failwith "Module: todo"
    | _ :: tl -> iter_toplvl_stmts tl
  in

	ignore iter_toplvl_stmts;
	ignore ast;
	ignore import_deps;

	failwith "todo"
