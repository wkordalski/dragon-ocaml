type node = Node.node

type token =
| Terminal of (int)
| Nonterminal of (int)

module Token =
struct
  type t = token
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
end

module TokenSet = Set.Make(Token)
module TokenTbl = Hashtbl.Make(Token)

type rule = Rule of (token * token list * (node list -> node))
type item = Item of (rule * token list)

module IRule =
struct
  type t = rule
  let compare a b =
    let Rule(x, k, _), Rule(y, l, _) = a, b in
    let c = Token.compare x y in
    if c <> 0 then c else
    let d = Pervasives.compare (List.length k) (List.length l) in
    if d <> 0 then d else
    List.fold_left2 (fun a x y -> if a <> 0 then a else Token.compare x y) 0 k l
end

module IItem =
struct
  type t = item
  let compare a b =
    let Item(x, p), Item(y, q) = a, b in
    let c = IRule.compare x y in
    if c <> 0 then c else
    Pervasives.compare (List.length p) (List.length q)
end

module ItemSet = Set.Make(IItem)

type item_set = ItemSet.t

let parse rules start =
  let rules_by_symbol = TokenTbl.create 100 in
  (* Adds necesery items to set *)
  let complete_item_set s =
    let rec helper l acc =
      match l with
      | [] -> acc
      | h::t ->
          let acc = List.fold_left (fun acc x -> ItemSet.add x acc) acc (TokenTbl.find rules_by_symbol h) in
          helper t acc
    in helper (ItemSet.fold (fun i a -> match i with Item(_, []) -> a | Item(_, h::_) -> h::a) s []) s
  in
  (
    (* Fill-in rules_by_symbol *)
    List.iter (fun r -> match r with
      Rule(s, _, _) when TokenTbl.mem rules_by_symbol s ->
        let rule_list = TokenTbl.find rules_by_symbol s
        in TokenTbl.replace rules_by_symbol s (r::rule_list)
    | Rule(s, _, _) -> TokenTbl.add rules_by_symbol s [r]
    ) rules;
    ()
  )
