module Err : sig
  open Token

  type err_type =
    | Fatal
    | Expect
    | Exhausted_tokens
    | Unknown_token
    | Malformed_proc_def
    | Unreachable
    | Unimplemented
    | Redeclaration
    | Undeclared
    | Syntax
    | Missing_binding
    | Missing_type

  val err : err_type -> string -> string -> ?msg:string -> Token.t option -> unit
end
