(* MIT License

   * Copyright (c) 2023 malloc-nbytes

   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:

   * The above copyright notice and this permission notice shall be included in all
   * copies or substantial portions of the Software.

   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   * SOFTWARE. *)

module Scope = struct
  open Ast
  open Err
  open Token

  type state =
    { mutable func_section : string
    ; mutable data_section : string
    ; mutable type_section : string
    ; mutable imports      : string list
    ; mutable cur_proc_id  : string * TokenType.id_type
    ; mutable compiled_files : string list
    }

  let state =
    { func_section = "";
      data_section = "";
      type_section = "";
      imports = [];
      cur_proc_id = "", TokenType.Void;
      compiled_files = [];
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

  type def_proc =
    { id : string
    ; params : TokenType.id_type list
    ; rettype : TokenType.id_type
    }

  type structure =
    { id : string
    (* name * type * offset *)
    ; members : (Token.t * TokenType.id_type * int) list
    ; size : int
    }

  let id_tbl : (((string, var) Hashtbl.t) list) ref
    = ref @@ Hashtbl.create 20 :: []

  let proc_tbl : (string, proc) Hashtbl.t ref = ref @@ Hashtbl.create 20

  let def_proc_tbl : (string, def_proc) Hashtbl.t ref = ref @@ Hashtbl.create 20

  let struct_tbl : (string, structure) Hashtbl.t ref = ref @@ Hashtbl.create 20

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
      | [] -> failwith @@ Printf.sprintf "get_token_from_scope: unreachable: %s" id
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
    ignore id;
    ignore args;
    failwith "assert_proc_args_match: unimplemented"

  let def_proc_tbl_add (id : string) (params : TokenType.id_type list) (rettype : TokenType.id_type) : unit =
    if Hashtbl.mem !def_proc_tbl id then
      let _ = Err.err Err.Redeclaration __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "redeclared function `%s`" id)
                None in exit 1
    else
      Hashtbl.add !def_proc_tbl id {id; params; rettype}

  let check_def_proc_in_tbl (id : string) : bool =
    Hashtbl.mem !def_proc_tbl id

  let get_def_proc_from_tbl (id : string) : def_proc =
    Hashtbl.find !def_proc_tbl id

  let assert_def_proc_not_in_tbl (id : string) : unit =
    if Hashtbl.mem !def_proc_tbl id then
      let _ = Err.err Err.Redeclaration __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "redeclared function `%s`" id)
                None in exit 1

  let add_struct_to_tbl (id : string) (members : (Token.t * TokenType.id_type) list) (size : int) : unit =
    if Hashtbl.mem !struct_tbl id then
      let _ = Err.err Err.Redeclaration __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "redeclared struct `%s`" id)
                None in exit 1
    else
      let rec add_struct_to_tbl' (members : (Token.t * TokenType.id_type) list) (offset : int) : (Token.t * TokenType.id_type * int) list =
        match members with
        | [] -> []
        | (tok, type_) :: ms ->
           let size = match type_ with
             | TokenType.I32 -> 4
             | TokenType.Char -> 1
             | TokenType.Usize -> 4 (* temporary *)
             | TokenType.Str -> 8
             | TokenType.Array _ -> 8
             | TokenType.Pointer _ -> 8
             | _ -> failwith "add_struct_to_tbl': unreachable" in
           let new_offset = offset + size in
           (tok, type_, offset) :: add_struct_to_tbl' ms new_offset in
      let members = add_struct_to_tbl' members 0 in
      Hashtbl.add !struct_tbl id {id; members; size}

  let get_struct_from_tbl (id : string) : structure =
    if not (Hashtbl.mem !struct_tbl id) then
      let _ = Err.err Err.Undeclared __FILE__ __FUNCTION__
                ~msg:(Printf.sprintf "undeclared struct `%s`" id)
                None in exit 1
    else
      Hashtbl.find !struct_tbl id

end
