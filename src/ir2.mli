module Ir2 : sig
  open Ast
  open Scope

  val generate_inter_lang : Ast.program -> string
end
