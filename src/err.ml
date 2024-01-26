module Err : sig
  type err_type =
    | SyntaxError

  val err : string -> string -> int -> string -> unit
end = struct
  type err_type =
    | SyntaxError

  let err file func line msg =
    Printf.eprintf "[%s:%s%d]: %s" file func line msg
  ;;
end
