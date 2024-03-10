module Scope : sig
  open Err
  open Token

  val tbl : (((string, (Token.t * (TokenType.id_type))) Hashtbl.t) list) ref

  val push : unit -> unit
  val pop : unit -> unit

  val assert_id_not_in_scope : string -> unit
  val assert_token_not_in_scope : Token.t -> unit
  val assert_token_in_scope : Token.t -> unit
  val assert_id_in_scope : string -> unit
  val add_id_to_scope : string -> Token.t -> TokenType.id_type -> unit
  val get_token_from_scope : string -> Token.t * (TokenType.id_type)
end
