module Proc : sig
  open Ast

  val populate_proc_tbl : Ast.program -> string list
end
