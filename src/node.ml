exception ParserMatchFailed

type str_t = Token.str_t

type types = [
| `TypeType
| `TemplateType
| `DefinedType
| `TypeTemplateInstantiation
]

type identifier = [
| `Identifier of str_t
]

type templates = [
| `IntegralType
| `DefinedTypeTemplate
]

type ctvalue = [
| types
| templates
]

type some_kind_of_identifier = [
| `Identifier of str_t
| ctvalue
]
