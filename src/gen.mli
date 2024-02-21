module Gen : sig
  open Ast
  val generate_bytecode : Ast.program -> string
end
