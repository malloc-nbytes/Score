module Lexer : sig
  open Token
  val lex_file : string -> Token.t list
  val print_tokens : Token.t list -> unit
end