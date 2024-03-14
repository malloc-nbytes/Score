(* ===============================
 * --- NOTE ---
 * FILE IS DEPRECATED AND WILL
 * BE DELETED IN DUE TIME.
 * SEE ir.ml INSTEAD
 * =============================== *)

module Ir : sig
  open Ast
  val generate_inter_lang : Ast.program -> string
end
