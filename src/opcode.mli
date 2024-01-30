module Opcode : sig
  type byte = int

  type t =
    | Halt
    | Nop
    | IPush
    | IAdd
    | ISub
    | IMult
    | IDiv
    | Hole

  val get_instr : t -> int

end
