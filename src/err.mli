module Err : sig
  open Token

  type err_type =
    | ParserFatalErr
    | ParserExpectErr
    | ParserExhaustedTokensErr
    | ParserUnknownTokenErr
    | ParserMalformedFuncDef

  val err : err_type -> string -> string -> ?msg:string -> Token.t option -> unit
end
