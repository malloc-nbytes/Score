module Preprocessor = struct
  open Token

  let rec gather_until_end
            (tokens : Token.t list)
            (acc : Token.t list)
          : Token.t list * Token.t list =
    match tokens with
    | Token.{ttype = TokenType.End} :: tl -> acc, tl
    | hd :: tl -> gather_until_end tl (acc @ [hd])
    | _ -> failwith @@ Printf.sprintf "%s: no more tokens" __FUNCTION__

  let print_hashtable (tbl : (string, Token.t list) Hashtbl.t) : unit =
    Hashtbl.iter (fun key value ->
        Printf.printf "Key: %s\n" key;
        List.iter (fun token -> Printf.printf "  %s\n" (Token.to_string token)) value;
        Printf.printf "\n"
      ) tbl

  let rec gather_macros
            (tokens : Token.t list)
            (tbl : (string, Token.t list) Hashtbl.t)
          : Token.t list =
    match tokens with
    | [] -> []
    | {ttype = TokenType.Macro; _} :: name :: tl ->
       let gathered, tokens = gather_until_end tl [] in
       Hashtbl.add tbl name.lexeme gathered;
       gather_macros tokens tbl
    | hd :: tl when Hashtbl.mem tbl hd.lexeme ->
       let repl = Hashtbl.find tbl hd.lexeme in
       gather_macros (repl @ tl) tbl
    | hd :: tl -> hd :: gather_macros tl tbl


  let insert_macros (tokens : Token.t list) : Token.t list =
    gather_macros tokens (Hashtbl.create 20)


end
