open Token

type variable =
  { id : Token.t
  ; type_ : TokenType.id_type
  }

type procedure =
  { id : Token.t
  ; params : (Token.t * TokenType.id_type) list
  ; rettype : TokenType.id_type
  }

type structure =
  { id : Token.t
  (* name * type * offset *)
  ; fields : (Token.t * TokenType.id_type * int) list
  ; size : int
  }

type t =
  { variables : ((string, variable) Hashtbl.t) list
  ; procedures : (string, procedure) Hashtbl.t
  ; structures : (string, structure) Hashtbl.t
  }

let create (module_ : Module.t) : t =
  let variables = Hashtbl.create 20 :: []
  and procedures = Hashtbl.create 20
  and structures = Hashtbl.create 20 in

  List.iter (fun (proc : Ast.proc_def_stmt) -> 
    let id = proc.id
    and params = failwith "todo"
    and rettype = proc.rettype in
    Hashtbl.add procedures proc.id.lexeme {id; params; rettype}
  ) module_.exported_procs;
  
  List.iter (fun (struct_ : Ast.struct_stmt) -> 
    let id = struct_.id
    and fields = failwith "todo"
    and size = failwith "todo" in
    Hashtbl.add structures struct_.id.lexeme {id; fields; size}
  ) module_.exported_types;

  {variables; procedures; structures}
