module Scope : sig
  open Token
  open Ast

  type state =
    { mutable func_section : string
    ; mutable data_section : string
    ; mutable type_section : string
    ; mutable cur_proc_id  : string * TokenType.id_type
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

  val state : state

  val id_tbl : (((string, var) Hashtbl.t) list) ref
  val proc_tbl : (string, proc) Hashtbl.t ref

  val push : unit -> unit
  val pop : unit -> unit

  val assert_id_not_in_scope : string -> unit
  val assert_token_not_in_scope : Token.t -> unit
  val assert_token_in_scope : Token.t -> unit
  val assert_id_in_scope : string -> unit
  val add_id_to_scope : string -> Token.t -> TokenType.id_type -> bool -> unit
  val modify_token_in_scope : string -> string option -> Token.t option -> TokenType.id_type option -> bool option -> unit
  val get_token_from_scope : string -> var
  val add_proc_to_tbl : Ast.proc_def_stmt -> unit
  val assert_proc_in_tbl : string -> unit
  val get_proc_rettype_from_tbl : string -> TokenType.id_type
  val assert_proc_args_match : string -> (Token.t * TokenType.id_type) list -> unit
end
