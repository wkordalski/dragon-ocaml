type char_t = char
type str_t = string

type t =
(*               text      *)
| Identifier of (str_t)
(*             text      *)
| Operator of (str_t)
(*           text      *)
| Number of (str_t)
(*         text      *)
| Text of (str_t)
| Newline
| Indent
| Dedent
| KeywordVar


let is_terminal n =
  match n with
  | Identifier _ -> true
  | Operator _ -> true
  | Number _ -> true
  | Text _ -> true
  | Newline -> true
  | Indent -> true
  | Dedent -> true
  | _ -> false

let identifier = 1
let operator = 2
let number = 3
let text = 4
let newline = 5
let indent = 6
let dedent = 7
