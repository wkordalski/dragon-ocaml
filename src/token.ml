type t =
| Terminal of (int)
| Nonterminal of (int)
| Empty

let compare a b =
  match (a,b) with
  | Terminal(x), Terminal(y) -> Pervasives.compare x y
  | Nonterminal(x), Nonterminal(y) -> Pervasives.compare x y
  | Empty, Empty -> 0
  | Terminal(x), Nonterminal(y) -> -1
  | Nonterminal(x), Terminal(y) -> 1
  | Empty, _ -> 1
  | _, Empty -> -1

let equal a b = (compare a b = 0)

let hash a =
  match a with
  | Terminal(x)    -> 1000007 * x
  | Nonterminal(y) -> 5186191 * y
  | Empty          -> 34616443

let is_terminal t =
  match t with
  | Terminal _ -> true
  | _ -> false

let is_nonterminal t =
  match t with
  | Nonterminal _ -> true
  | _ -> false

let is_empty t =
  match t with
  | Empty -> true
  | _ -> false

let empty = Empty

let print t =
  match t with
  | Terminal(x) -> print_int x
  | Nonterminal(x) -> print_int x
  | Empty -> print_string "<>"


