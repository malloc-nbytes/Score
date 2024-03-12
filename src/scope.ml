module Scope = struct
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
    { id : string
    ; token : Token.t
    ; type_ : TokenType.id_type
    ; stack_allocd : bool
    }

  let id_tbl : (((string, var) Hashtbl.t) list) ref
    = ref @@ Hashtbl.create 20 :: []

  let func_tbl : (string, var list) Hashtbl.t ref
    = ref @@ Hashtbl.create 20

  let push () : unit = id_tbl := Hashtbl.create 20 :: !id_tbl

  let pop () : unit = id_tbl := List.tl !id_tbl

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

  let add_func_to_scope (id : string) (params : (Token.t * TokenType.id_type) list) : unit =
    failwith "add_func_to_scope: unimplemented"
(*
    Hashtbl.add !func_tbl id params
*)

  let assert_func_params_match (id : string) (params : (Token.t * TokenType.id_type) list) : unit =
    failwith "assert_func_params_match: unimplemented"

end
