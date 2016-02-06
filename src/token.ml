type char_t = char
type str_t = string

type t =
| Identifier of (str_t)
| Keyword of (str_t)
| Operator of (str_t)
| Number of (str_t)
| Newline
| Indent
| Dedent
| Unset
| End
(* Lexer-internal tokens *)
| OperatorLineComment
| OperatorBlockComment
| OperatorNestableComment
| OperatorLineJoiner
(* Comments *)
| LineComment of (str_t)
| BlockComment of (str_t)
| NestedComment of (str_t)
(* Parenthesis *)
| ParenRoundLeft
| ParenRoundRight
| ParenSquareLeft
| ParenSquareRight
| ParenCurlyLeft
| ParenCurlyRight
(* String literals *)
| MultilineUsualStringLiteral of (str_t)
| MultilineAugumentedStringLiteral of (str_t)
| MultilineWysiwygStringLiteral of (str_t)
| UsualStringLiteral of (str_t)
| AugumentedStringLiteral of (str_t)
| WysiwygStringLiteral of (str_t)

let print n =
  match n with
  | Identifier(s) ->print_string ("["^s^"]")
  | Keyword(s) ->print_string ("<"^s^">")
  | Operator(s) -> print_string ("["^s^"]")
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
  | End -> print_string "$"
  | _ -> print_string "#"

let equal a b =
  a = b
