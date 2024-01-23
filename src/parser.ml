module Parser = struct
  open Token
  open Ast

  (* Takes some option and attempts
   * to unwrap it, returning the inner value.
   * Will panic if `k` is None. *)
  let unwrap k =
    match k with
    | Some k' -> k'
    | None -> failwith "unwrapped None value"
  ;;

  (* Takes a list of tokens and an expected token type.
   * If the type of the head of the list does not match `exp`,
   * it will fail. It will also fail if `lst` is empty. Will
   * return the head and tail split from each other. This function
   * should be used instead of `pop ()` when we want to assure a
   * specific type *)
  let expect (lst : Token.t list) (exp : TokenType.t) : Token.t * Token.t list =
    match lst with
    | [] -> failwith "call to expect () with an empty list"
    | hd :: _ when hd.ttype <> exp -> failwith "call to expect () failed with differing types"
    | hd :: tl -> hd, tl
  ;;

  (* Takes a list and discards the head of it
   * but returns the tail of it. Should be used
   * when wanting to discard the head. *)
  let rem (lst : Token.t list) : Token.t list =
    match lst with
    | [] -> failwith "called rem () with no tokens"
    | _ :: tl -> tl
  ;;

  (* Takes a list and splits the head from the tail
   * and returns both. Should be used when wanting to
   * consume the head, but still use it. *)
  let pop (lst : Token.t list) : Token.t * Token.t list =
    match lst with
    | [] -> failwith "called pop () with no tokens"
    | hd :: tl -> hd, tl
  ;;

  let rec produce_program (tokens : Token.t list) : Ast.node_prog =
    failwith "todo"
  ;;

end
