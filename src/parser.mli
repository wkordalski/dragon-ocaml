type token =
| Terminal of (int)
| Nonterminal of (int)

type rule = Rule of (token * token list * (Node.node list -> Node.node))
type item = Item of (rule * token list)

val parse : rule list -> rule -> unit
