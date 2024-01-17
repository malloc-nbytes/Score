module Lexer : sig
  open Token
  val populate_keywords : unit -> unit
  val lex_file : char list -> int -> int -> Token.t list
  val print_tokens : Token.t list -> unit
end
