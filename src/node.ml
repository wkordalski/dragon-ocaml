type char_t = char
type str_t = string

type node =
(*               text      *)
| Identifier of (str_t)
(*             text      *)
| Operator of (str_t)
(*            text      *)
| Literal of (str_t)
| Newline
| Indent
| Dedent
