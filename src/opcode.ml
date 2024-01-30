module Opcode = struct
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

  let get_instr (opcode : t) : byte =
    match opcode with
    | Halt -> 0x01
    | Nop -> 0x02
    | IPush -> 0x03
    | IAdd -> 0x04
    | ISub -> 0x05
    | IMult -> 0x06
    | IDiv -> 0x07
    | Hole -> 0xFF

end
