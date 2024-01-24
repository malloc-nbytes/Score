module Parser : sig
  open Token
  open Ast

  val produce_ast : Token.t list -> Ast.node_prog
  val print_ast : Ast.node_prog -> unit
end
