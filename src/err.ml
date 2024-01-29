module Err = struct
  open Token

  type err_type =
    | Fatal
    | Expect
    | Exhausted_tokens
    | Unknown_token
    | Malformed_func_def
    | Unreachable

  let err_to_str (err_type : err_type) : string =
    match err_type with
    | Fatal -> "Fatal"
    | Expect -> "Expect"
    | Exhausted_tokens -> "Exhausted_tokens"
    | Unknown_token -> "Unknown_token"
    | Malformed_func_def -> "Malformed_func_def"
    | Unreachable -> "Unreachable"

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
