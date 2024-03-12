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

  let id_tbl : (((string, (Token.t * TokenType.id_type)) Hashtbl.t) list) ref
    = ref @@ Hashtbl.create 20 :: []

  let func_tbl : (string, (Token.t * TokenType.id_type) list) Hashtbl.t ref
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

  let add_id_to_scope (id : string) (token : Token.t) (type_ : TokenType.id_type) : unit =
    let s = List.hd !id_tbl in
    Hashtbl.add s id (token, type_)

  let get_token_from_scope (id : string) : Token.t * (TokenType.id_type) =
    let rec get_token_from_scope' (tbl : ((string, (Token.t * (TokenType.id_type))) Hashtbl.t) list)
            : Token.t * (TokenType.id_type) =
      match tbl with
      | [] -> failwith "unreachable"
      | s :: ss ->
         if Hashtbl.mem s id then
           Hashtbl.find s id
         else
           get_token_from_scope' ss in
    get_token_from_scope' !id_tbl

  let add_func_to_scope (id : string) (params : (Token.t * TokenType.id_type) list) : unit =
    Hashtbl.add !func_tbl id params

  let assert_func_params_match (id : string) (params : (Token.t * TokenType.id_type) list) : unit =
    let expected_params = Hashtbl.find !func_tbl id in
    if List.length expected_params <> List.length params then
      let _ = Err.err Err.Fatal __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "expected %d arguments, got %d"
                        (List.length expected_params) (List.length params))
                         None in let _ = exit 1 in
    List.iter2 (fun (t1, orig) (t2, _) ->
        if t1.Token.ttype <> t2.Token.ttype then
          let _ = Err.err Err.Fatal __FILE__ __FUNCTION__
                    ~msg:(Printf.sprintf "expected type %s, got type %s"
                            (Token.to_string t1)
                            (Token.to_string t2))
                    None in exit 1) expected_params params

end
