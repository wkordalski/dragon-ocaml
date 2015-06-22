
type t =
| Terminal of (int)
| Nonterminal of (int)
| Empty
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

val is_terminal : t -> bool
val is_nonterminal : t -> bool

val empty : t
val is_empty : t -> bool

val print : t -> unit