module Parser : sig
  open Token
  open Ast

  val produce_program : Token.t list -> Ast.node_prog
end
