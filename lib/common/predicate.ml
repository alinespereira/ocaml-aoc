let is_digit ch =
  let code = Char.code ch in
  Char.code '0' <= code && code <= Char.code '9'
;;
