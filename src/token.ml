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

let to_string n =
  match n with
  | Identifier(s) -> ("["^s^"]")
  | Keyword(s) -> ("<"^s^">")
  | Operator(s) -> ("["^s^"]")
  | Indent -> "<INDENT>"
  | Dedent -> "<DEDENT>"
  | BlockComment(s) -> ("{"^s^"}")
  | NestedComment(s) -> ("{"^s^"}")
  | LineComment(s) -> ("{"^s^"}")
  | UsualStringLiteral(s) -> ("["^s^"]")
  | AugumentedStringLiteral(s) -> ("["^s^"]")
  | WysiwygStringLiteral(s) -> ("["^s^"]")
  | MultilineUsualStringLiteral(s) -> ("["^s^"]")
  | MultilineAugumentedStringLiteral(s) -> ("["^s^"]")
  | MultilineWysiwygStringLiteral(s) -> ("["^s^"]")
  | ParenRoundLeft -> "<(>"
  | ParenRoundRight -> "<)>"
  | ParenSquareLeft -> "<[>"
  | ParenSquareRight -> "<]>"
  | ParenCurlyLeft -> "<{>"
  | ParenCurlyRight -> "<}>"
  | Newline -> "<\\n>"
  | End -> "$"
  | _ -> "#"

let print n =
  print_string (to_string n)

let equal a b =
  a = b

let is_string n =
  match n with
  | UsualStringLiteral _ -> true
  | AugumentedStringLiteral _ -> true
  | WysiwygStringLiteral _ -> true
  | MultilineUsualStringLiteral _ -> true
  | MultilineAugumentedStringLiteral _ -> true
  | MultilineWysiwygStringLiteral _ -> true
  | _ -> false

let get_string n =
  match n with
  | UsualStringLiteral(s) -> s
  | AugumentedStringLiteral(s) -> s
  | WysiwygStringLiteral(s) -> s
  | MultilineUsualStringLiteral(s) -> s
  | MultilineAugumentedStringLiteral(s) -> s
  | MultilineWysiwygStringLiteral(s) -> s
  | _ -> ""

let token_pretty_printer formatter token =
  Format.pp_print_string formatter (to_string token)
