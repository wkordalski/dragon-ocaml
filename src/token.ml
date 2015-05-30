
type t =
| Terminal of (int)
| Nonterminal of (int)

let compare a b =
  match (a,b) with
  | Terminal(x), Terminal(y) -> Pervasives.compare x y
  | Nonterminal(x), Nonterminal(y) -> Pervasives.compare x y
  | Terminal(x), Nonterminal(y) -> -1
  | Nonterminal(x), Terminal(y) -> 1

let equal a b = (compare a b = 0)

let hash a =
  match a with
  | Terminal(x)    -> 1000007 * x
  | Nonterminal(y) -> 5186191 * y