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

module Scope : sig
  open Token
  open Ast

  type state =
    { mutable func_section : string
    ; mutable data_section : string
    ; mutable type_section : string
    ; mutable imports      : string list
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

  val state : state

  val id_tbl : (((string, var) Hashtbl.t) list) ref
  val proc_tbl : (string, proc) Hashtbl.t ref
  val def_proc_tbl : (string, def_proc) Hashtbl.t ref
  val struct_tbl : (string, structure) Hashtbl.t ref

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
  val def_proc_tbl_add : string -> TokenType.id_type list -> TokenType.id_type -> unit
  val check_def_proc_in_tbl : string -> bool
  val get_def_proc_from_tbl : string -> def_proc
  val assert_def_proc_not_in_tbl : string -> unit
  val add_struct_to_tbl : string -> (Token.t * TokenType.id_type) list -> int -> unit
  val get_struct_from_tbl : string -> structure
end
