module Preprocessor : sig
  open Token

  val insert_macros : Token.t list -> Token.t list
end
