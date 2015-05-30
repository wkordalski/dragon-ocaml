
type t =
| Terminal of (int)
| Nonterminal of (int)

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
