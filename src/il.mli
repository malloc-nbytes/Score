module Il : sig
  open Ast
  val generate_inter_lang : Ast.program -> string
end