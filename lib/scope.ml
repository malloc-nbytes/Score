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

let dump (scope : t) : unit =
  let open Printf in

  let rec aux (s : (string, variable) Hashtbl.t list) depth =
    match s with
    | [] -> ()
    | hd :: tl ->
      Hashtbl.iter (fun (key : string) (value : variable) -> printf "  %s -> variable(%s), depth(%d)\n" key value.id.lexeme depth) hd;
      aux tl (depth+1) in

  printf "variables:\n";
  aux scope.variables 0;
  printf "procedures:\n";
  Hashtbl.iter (fun (key : string) (value : procedure) -> printf "  %s -> procedure(%s)\n" key value.id.lexeme) scope.procedures;
  printf "structures:\n";
  Hashtbl.iter (fun (key : string) (value : structure) -> printf "  %s -> structure(%s)\n" key value.id.lexeme) scope.structures

let create (module_ : Module.t) : t =
  let variables = Hashtbl.create 20 :: [] (* to be filled when traversing AST *)
  and procedures = Hashtbl.create 20
  and structures = Hashtbl.create 20 in

  List.iter (fun (proc : Ast.proc_def_stmt) ->
      let id = proc.id
      and params = List.map (fun (t, id) -> (t, id)) proc.params
      and rettype = proc.rettype in
      Hashtbl.add procedures proc.id.lexeme {id; params; rettype}
    ) module_.exported_procs;

  List.iter (fun (struct_ : Ast.struct_stmt) ->
      let rec aux (members : (Token.t * TokenType.id_type) list) (offset : int) : (Token.t * TokenType.id_type * int) list =
        match members with
        | [] -> []
        | (tok, type_) :: ms ->
          let size = match type_ with
                      | TokenType.I32 -> 4
                      | TokenType.Char -> 1
                      | TokenType.Usize -> 8
                      | TokenType.Str -> 8
                      | TokenType.Array _ -> 8
                      | TokenType.Pointer _ -> 8
                      | _ -> failwith @@ Printf.sprintf "%s:create: unreachable" __FILE__ in
          let new_offset = offset + size in
          (tok, type_, offset) :: aux ms new_offset in

      let id = struct_.id
      and fields = aux struct_.fields 0
      and size = failwith @@ Printf.sprintf "%s:create: unimplemented" __FILE__ in

      Hashtbl.add structures struct_.id.lexeme {id; fields; size}
    ) module_.exported_types;

  {variables; procedures; structures}
