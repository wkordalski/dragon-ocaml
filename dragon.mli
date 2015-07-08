type node = Node.t
type char_t = Node.char_t
type str_t = Node.str_t

val lex : char_t Stream.t -> node Stream.t
val print : node -> string
val parse : node Stream.t -> node list
val parse_stream : node Stream.t -> node list

val print_node : node -> unit
