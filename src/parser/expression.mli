type identifier = [
| `Identifier of Node.str_t
]

type expression = [
| identifier
| `GetMemberOperator of (expression * identifier)
| `PostfixIncreaseOperator of (expression)
| `PostfixDecreaseOperator of (expression)
]

val parse_expression : Token.t list -> (expression * Token.t list)
val parse_postfix_expression : Token.t list -> (expression * Token.t list)
val parse_primary_expression : Token.t list -> (expression * Token.t list)
