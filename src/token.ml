type t =
| Terminal of (int)
| Nonterminal of (int)
| Empty
| End
| Root

let special_id a =
  match a with
  | Terminal _ -> 0
  | Nonterminal _ -> 0
  | Empty -> 1
  | End -> 2
  | Root -> 3

let compare a b =
  match (a,b) with
  | Terminal(x), Terminal(y) -> Pervasives.compare x y
  | Nonterminal(x), Nonterminal(y) -> Pervasives.compare x y
  | Terminal(x), Nonterminal(y) -> -1
  | Nonterminal(x), Terminal(y) -> 1
  | _, _ -> Pervasives.compare (special_id a) (special_id b)

let equal a b = (compare a b = 0)

let hash a =
  match a with
  | Terminal(x)    -> 1000007 * x
  | Nonterminal(y) -> 5186191 * y
  | Empty          -> 34616443
  | End            -> 550427
  | Root           -> 717323

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

let is_ending t =
  match t with
  | End -> true
  | _ -> false

let is_root t =
  match t with
  | Root -> true
  | _ -> false

let ending = End

let empty = Empty

let root = Root

let print t =
  match t with
  | Terminal(x) -> print_int x
  | Nonterminal(x) -> print_int x
  | Empty -> print_string "<>"
  | End -> print_string "$"
  | Root -> print_string "@"


