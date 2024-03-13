module Utils : sig
  open Token

  val unwrap : 'a option -> 'a
  val file_to_str : string -> string
  val write_to_file : string -> string -> unit
  val scr_type_to_bytes : TokenType.id_type -> string
  val scr_to_qbe_type : TokenType.id_type -> string
  val unwrap_ptr : TokenType.id_type -> TokenType.id_type
end