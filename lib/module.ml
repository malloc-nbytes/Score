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

let rec iter_toplvl_stmts (ast : Ast.program) (asts : (string, Ast.program) Hashtbl.t) (import_deps : string list) : t =
  let rec aux
            (stmts : Ast.toplvl_stmt list)
            (modname : string)
            (depends : t list)
            (exported_procs : Ast.proc_def_stmt list)
            (exported_types : Ast.struct_stmt list) : t =
    match stmts with
    | [] ->
       if modname <> "" then
         let depends = List.map (fun dep -> iter_toplvl_stmts (Hashtbl.find asts dep) asts []) import_deps in
         {modname; ast; depends; exported_procs; exported_types}
       else failwith @@ Printf.sprintf "%s: no module name found" __FILE__
    | Ast.Module m :: tl -> aux tl m.id.lexeme depends exported_procs exported_types
    | (Ast.Proc_def pd :: tl) when pd.export -> aux tl modname depends (pd :: exported_procs) exported_types
    | Ast.Struct _ :: _ -> failwith "iter_toplvl_stmts: Ast.Struct is unimplemented"
    | _ :: tl -> aux tl modname depends exported_procs exported_types in
  aux ast "" [] [] []

and produce_modules (asts : (string, Ast.program) Hashtbl.t) (import_deps : (string, string list) Hashtbl.t) : t list =
  let rec aux = function
    | [] -> []
    | (filepath, imports) :: tl ->
       let ast = Hashtbl.find asts filepath in
       let module_ = iter_toplvl_stmts ast asts imports in
       module_ :: aux tl in
  aux (Hashtbl.fold (fun k v acc -> (k, v) :: acc) import_deps [])

