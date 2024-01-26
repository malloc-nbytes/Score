module Err = struct
  open Token

  type err_type =
    | ParserFatalErr
    | ParserExpectErr
    | ParserExhaustedTokensErr
    | ParserUnknownTokenErr
    | ParserMalformedFuncDef

  let err_to_str (err_type : err_type) : string =
    match err_type with
    | ParserFatalErr -> "ParserFatalErr"
    | ParserExpectErr -> "ParserExpectErr"
    | ParserExhaustedTokensErr -> "ParserExhaustedTokensErr"
    | ParserUnknownTokenErr -> "ParserUnknownTokenErr"
    | ParserMalformedFuncDef -> "ParserMalformedFuncDef"

  let err (err_type : err_type) (file : string) (func : string)
        ?(msg="") (token : Token.t option) : unit =
    let s = err_to_str err_type in
    match token with
    | Some token' ->
       let msg = (if msg = "" then msg else "[ERR] " ^ msg ^ "\n") in
       Printf.eprintf
         "[ERR] %s [%s:%s]:\n%s[ERR] conflicting token: %s\n"
         s file func msg (Token.to_string token')
    | None -> Printf.eprintf "[ERR] %s [%s:%s]:\n[ERR] %s\n" s file func msg
  ;;

end
