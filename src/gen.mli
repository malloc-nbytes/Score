module Gen : sig
  open Ast
  open Opcode
  val generate_bytecode : Ast.program -> Opcode.byte list
end
