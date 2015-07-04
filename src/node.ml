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
| OperatorPlus
| OperatorSlash
| OperatorPlusPlus
| OperatorLineComment
| OperatorBlockComment
| OperatorNestableComment
| LineComment of (str_t)
| BlockComment of (str_t)
| NestedComment of (str_t)
| ParenRoundLeft
| ParenRoundRight
| ParenSquareLeft
| ParenSquareRight
| ParenCurlyLeft
| ParenCurlyRight
| MultilineUsualStringLiteral of (str_t)
| MultilineAugumentedStringLiteral of (str_t)
| MultilineWysiwygStringLiteral of (str_t)
| UsualStringLiteral of (str_t)
| AugumentedStringLiteral of (str_t)
| WysiwygStringLiteral of (str_t)
| OperatorLineJoiner


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

let print n =
  match n with
  | Identifier(s) ->print_string ("["^s^"]")
  | OperatorPlus -> print_string "[+]"
  | OperatorPlusPlus -> print_string "[++]"
  | Indent -> print_string "<INDENT>"
  | Dedent -> print_string "<DEDENT>"
  | BlockComment(s) -> print_string ("{"^s^"}")
  | NestedComment(s) -> print_string ("{"^s^"}")
  | LineComment(s) -> print_string ("{"^s^"}")
  | UsualStringLiteral(s) -> print_string ("["^s^"]")
  | AugumentedStringLiteral(s) -> print_string ("["^s^"]")
  | WysiwygStringLiteral(s) -> print_string ("["^s^"]")
  | MultilineUsualStringLiteral(s) -> print_string ("["^s^"]")
  | MultilineAugumentedStringLiteral(s) -> print_string ("["^s^"]")
  | MultilineWysiwygStringLiteral(s) -> print_string ("["^s^"]")
  | ParenRoundLeft -> print_string "<(>"
  | ParenRoundRight -> print_string "<)>"
  | ParenSquareLeft -> print_string "<[>"
  | ParenSquareRight -> print_string "<]>"
  | ParenCurlyLeft -> print_string "<{>"
  | ParenCurlyRight -> print_string "<}>"
  | Newline -> print_string "<\\n>"
  | _ -> ()
