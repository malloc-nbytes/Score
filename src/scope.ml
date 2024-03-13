module Scope = struct
  open Ast
  open Err
  open Token

  type state =
    { mutable func_section : string
    ; mutable data_section : string
    ; mutable type_section : string
    ; mutable cur_proc_id  : string * TokenType.id_type
    }

  let state =
    {func_section = "";
     data_section = "";
     type_section = "";
     cur_proc_id = "", TokenType.Void
    }

  type var =
    { mutable id : string
    ; mutable token : Token.t
    ; mutable type_ : TokenType.id_type
    ; mutable stack_allocd : bool
    }

  type proc =
    { id : string
    ; params : (Token.t * TokenType.id_type) list
    ; rettype : TokenType.id_type
    }

  let id_tbl : (((string, var) Hashtbl.t) list) ref
    = ref @@ Hashtbl.create 20 :: []

  let proc_tbl : (string, proc) Hashtbl.t ref = ref @@ Hashtbl.create 20

  let push () : unit = id_tbl := Hashtbl.create 20 :: !id_tbl

  let pop () : unit = id_tbl := List.tl !id_tbl

  (*** Variables ***)

  let assert_token_not_in_scope (token : Token.t) : unit =
    if List.exists (fun s -> Hashtbl.mem s token.lexeme) !id_tbl then
      let _ = Err.err Err.Redeclaration __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "redeclared identifier `%s`" token.lexeme)
                (Some token) in exit 1

  let assert_id_not_in_scope (id : string) : unit =
    if List.exists (fun s -> Hashtbl.mem s id) !id_tbl then
      let _ = Err.err Err.Redeclaration __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "redeclared identifier `%s`" id)
                None in exit 1

  let assert_token_in_scope (token : Token.t) : unit =
    if not (List.exists (fun s -> Hashtbl.mem s token.lexeme) !id_tbl) then
      let _ = Err.err Err.Undeclared __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "undeclared identifier `%s`" token.lexeme)
                (Some token) in exit 1

  let assert_id_in_scope (id : string) : unit =
    if not (List.exists (fun s -> Hashtbl.mem s id) !id_tbl) then
      let _ = Err.err Err.Undeclared __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "undeclared identifier `%s`" id)
                None in exit 1

  let add_id_to_scope (id : string) (token : Token.t) (type_ : TokenType.id_type) (stack_allocd : bool) : unit =
    let s = List.hd !id_tbl in
    Hashtbl.add s id {id; token; type_; stack_allocd}

  let get_token_from_scope (id : string) : var =
    let rec get_token_from_scope' (tbl : ((string, var) Hashtbl.t) list)
            : var =
      match tbl with
      | [] -> failwith "unreachable"
      | s :: ss ->
         if Hashtbl.mem s id then
           Hashtbl.find s id
         else
           get_token_from_scope' ss in
    get_token_from_scope' !id_tbl

  let modify_token_in_scope (orig_id : string) (new_id : string option) (new_token : Token.t option) (new_type : TokenType.id_type option) (new_stack_allocd : bool option) : unit =
    let rec modify_token_in_scope' (tbl : ((string, var) Hashtbl.t) list) : unit =
      match tbl with
      | [] -> failwith "unreachable"
      | s :: ss ->
         if Hashtbl.mem s orig_id then
           let v = Hashtbl.find s orig_id in
           let new_id = match new_id with
             | Some id -> id
             | None -> v.id in
           let new_token = match new_token with
             | Some token -> token
             | None -> v.token in
           let new_type = match new_type with
             | Some type_ -> type_
             | None -> v.type_ in
           let new_stack_allocd = match new_stack_allocd with
             | Some stack_allocd -> stack_allocd
             | None -> v.stack_allocd in
           Hashtbl.remove s orig_id;
           Hashtbl.add s new_id {id = new_id; token = new_token; type_ = new_type; stack_allocd = new_stack_allocd}
         else
           modify_token_in_scope' ss in
    modify_token_in_scope' !id_tbl

  (*** Procedures ***)

  let add_proc_to_tbl (pd : Ast.proc_def_stmt) : unit =
    let id = pd.id.lexeme in
    let params = List.map (fun (t, id) -> (t, id)) pd.params in
    let rettype = pd.rettype in
    if Hashtbl.mem !proc_tbl id then
      let _ = Err.err Err.Redeclaration __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "redeclared function `%s`" id)
                (Some pd.id) in exit 1
    else
      Hashtbl.add !proc_tbl id {id; params; rettype}

  let assert_proc_in_tbl (id : string) : unit =
    if not (Hashtbl.mem !proc_tbl id) then
      let _ = Err.err Err.Undeclared __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "undeclared function `%s`" id)
                None in exit 1

  let get_proc_rettype_from_tbl (id : string) : TokenType.id_type =
    let proc = Hashtbl.find !proc_tbl id in
    proc.rettype

  let assert_proc_args_match (id : string) (args : (Token.t * TokenType.id_type) list) : unit =
    failwith "assert_proc_args_match: unimplemented"

end
