module type TOKEN =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_terminal : t -> bool
  val is_nonterminal : t -> bool
end


type t =
| Terminal of (int)
| Nonterminal of (int)
| Empty

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

let is_terminal t =
  match t with
  | Terminal _ -> true
  | _ -> false

let is_nonterminal t =
  match t with
  | Nonterminal _ -> true
  | _ -> false
