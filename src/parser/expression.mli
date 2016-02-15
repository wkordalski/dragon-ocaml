type identifier = [
| `Identifier of Node.str_t
]

type expression = [
| identifier
| `GetMemberOperator of (expression * identifier)
| `PostfixIncreaseOperator of (expression)
| `PostfixDecreaseOperator of (expression)
| `PrefixIncreaseOperator of (expression)
| `PrefixDecreaseOperator of (expression)
| `PowerOperator of (expression * expression)
| `UnaryPlusOperator of (expression)
| `UnaryMinusOperator of (expression)
| `UnaryComplementOperator of (expression)
| `MultiplyOperator of (expression * expression)
| `DivideOperator of (expression * expression)
| `ModuloOperator of (expression * expression)
]

val parse_expression : Token.t list -> (expression * Token.t list)
