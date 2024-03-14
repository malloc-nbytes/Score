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
    | No_return
    | Type_mismatch

  val err : err_type -> string -> string -> ?msg:string -> Token.t option -> unit
  val err_type_mismatch : ?msg:string -> Token.t option -> TokenType.id_type -> TokenType.id_type -> unit
end
