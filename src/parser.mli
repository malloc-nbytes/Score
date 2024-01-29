module Parser : sig
  open Token
  open Ast

  val produce_ast : Token.t list -> Ast.program
end
