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

module Emit : sig
  open Token

  val proc_def : bool -> string -> (Token.t * TokenType.id_type) list -> TokenType.id_type -> unit
  val string_in_data_section : string -> string -> unit
  val stack_alloc4 : string -> string -> unit
  val stack_alloc8 : string -> string -> unit
  val stack_alloc16 : string -> string -> unit
  val store : string -> string -> TokenType.id_type -> unit
  val load : string -> TokenType.id_type -> string -> unit
  val __instr : string -> TokenType.id_type -> string -> string -> unit
  val rbrace : unit -> unit
  val copy : string -> TokenType.id_type -> string -> unit
  val extsb : string -> TokenType.id_type -> string -> unit
  val extsw : string -> TokenType.id_type -> string -> unit
  val extsh : string -> TokenType.id_type -> string -> unit
  val assignment : string -> TokenType.id_type -> string -> unit
  val binop : string -> TokenType.id_type -> string -> string -> string -> unit
  val ret : string -> string -> unit
  val proc_call_wassign : string -> string -> string -> TokenType.id_type -> unit
  val proc_call_woassign : string -> string -> unit
  val jnz : string -> string -> string -> unit
  val lbl : string -> unit
  val jmp : string -> unit
end
