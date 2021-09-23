exception Unimplemented of string
let unimplemented s = raise (Unimplemented s)

let p s = Printf.printf "%s" s