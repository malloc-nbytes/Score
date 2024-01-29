module Err : sig
  open Token

  type err_type =
    | Fatal
    | Expect
    | Exhausted_tokens
    | Unknown_token
    | Malformed_func_def
    | Unreachable

  val err : err_type -> string -> string -> ?msg:string -> Token.t option -> unit
end
