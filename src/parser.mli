
type rule = Rule of (Token.t * Token.t list * (Node.node list -> Node.node))
type item = Item of (rule * Token.t list)

val parse : rule list -> Token.t -> unit
