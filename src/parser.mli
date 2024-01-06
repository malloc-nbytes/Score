open Token
open Ast

module Parser : sig
  val parse_program : Token.t list -> Ast.node_func_stmt list
end
