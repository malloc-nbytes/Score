module Utils = struct

  let file_to_str filepath =
    let ch = open_in_bin filepath in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

  let write_to_file filepath content =
    let oc = open_out filepath in
    let _ = Printf.fprintf oc "%s" content in
    close_out oc

end
