module Preprocessor = struct
  open Token

  let rec gather_until_end
            (tokens : Token.t list)
            (acc : Token.t list)
            (macro_name : string)
          : Token.t list * Token.t list =
    match tokens with
    | Token.{ttype = TokenType.In; _} :: tl -> acc, tl
    | hd :: tl ->
       let hd = {hd with macro = Some macro_name} in
       gather_until_end tl (acc @ [hd]) macro_name
    | _ -> failwith @@ Printf.sprintf "%s: no more tokens" __FUNCTION__

  let rec gather_macros
            (tokens : Token.t list)
            (tbl : (string, Token.t list) Hashtbl.t)
          : Token.t list =
    match tokens with
    | [] -> []
    | {ttype = TokenType.Macro; _} :: name :: tl ->
       if Hashtbl.mem tbl name.lexeme then
         failwith @@ Printf.sprintf "redefined macro name: %s" name.lexeme
       else
         let gathered, tokens = gather_until_end tl [] name.lexeme in
         Hashtbl.add tbl name.lexeme gathered;
         gather_macros tokens tbl
    | hd :: tl when Hashtbl.mem tbl hd.lexeme ->
       let repl = Hashtbl.find tbl hd.lexeme in
       gather_macros (repl @ tl) tbl
    | hd :: tl -> hd :: gather_macros tl tbl

  let insert_macros (tokens : Token.t list) : Token.t list =
    gather_macros tokens (Hashtbl.create 20)

end
