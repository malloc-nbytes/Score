module Emit : sig
  open Token

  val proc_def : bool -> string -> (Token.t * TokenType.id_type) list -> TokenType.id_type -> unit
  val string_in_data_section : string -> string -> unit
  val stack_alloc4 : string -> string -> unit
  val stack_alloc8 : string -> string -> unit
  val stack_alloc16 : string -> string -> unit
  val store : string -> string -> TokenType.id_type -> unit
  val load : string -> TokenType.id_type -> string -> unit
  val __instr : string -> TokenType.id_type -> string -> string -> unit
  val rbrace : unit -> unit
  val copy : string -> TokenType.id_type -> string -> unit
  val extsb : string -> TokenType.id_type -> string -> unit
  val extsw : string -> TokenType.id_type -> string -> unit
  val extsh : string -> TokenType.id_type -> string -> unit
  val assignment : string -> TokenType.id_type -> string -> unit
  val binop : string -> TokenType.id_type -> string -> string -> string -> unit
  val ret : string -> unit
  val proc_call_wassign : string -> string -> string -> TokenType.id_type -> unit
  val proc_call_woassign : string -> string -> unit
end
