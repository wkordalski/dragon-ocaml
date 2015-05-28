type node = Node.node

type token =
| Terminal of (int)
| Nonterminal of (int)

module IToken =
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

module TokenSet = Set.Make(IToken)
module TokenMap = Map.Make(IToken)
module TokenTbl = Hashtbl.Make(IToken)

type rule = Rule of (token * token list * (node list -> node))
type item = Item of (rule * token list)

module IRule =
struct
  type t = rule
  let compare a b =
    let Rule(x, k, f), Rule(y, l, g) = a, b in
    let c = IToken.compare x y in
    if c <> 0 then c else
    let d = Pervasives.compare (List.length k) (List.length l) in
    if d <> 0 then d else
    let e = List.fold_left2 (fun a x y -> if a <> 0 then a else IToken.compare x y) 0 k l in
    if e <> 0 then e else
    if f <> g then assert false else 0
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
module ItemMap = Map.Make(IItem)

module IItemSet =
struct
  type t = ItemSet.t
  let compare = ItemSet.compare
end

module ItemSetMap = Map.Make(IItemSet)

let parse rules start =
  
  (* Token -> Rule list translation *)
  let rules_by_symbol t =
    let database : (rule list) TokenMap.t =
      let helper acc r =
        let Rule(t, _, _) = r in
        let l = try TokenMap.find t acc with Not_found -> [] in
        TokenMap.add t (r::l) acc 
      in List.fold_left helper TokenMap.empty rules
    in try TokenMap.find t database with Not_found -> []
  in
  
  (* Creates set with all rules definig t *)
  let itemset_by_token t =
    let rec helper l acc =
      match l with
      | [] -> acc
      | (Rule(_, p, _) as h)::t -> helper t (ItemSet.add (Item(h, p)) acc)
    in helper (rules_by_symbol t) (ItemSet.empty)
  in
  
  (* Returns list of expected tokens by itemset *)
  let expected_tokens_of_itemset s =
    let process_item i acc =
      match i with
      | Item(_, []) -> acc
      | Item(_, h::_) -> h::acc
    in
    ItemSet.fold process_item s []
  in
  
  (* Adds necesery items to set *)
  let complete_itemset s =
    (* items list to fulfill -> output items -> output items fulfilled *)
    let rec helper l acc =
      (* ItemSet -> Rule -> ItemSet *)
      let add_rule_to_itemset s r =
        let Rule(_, p, _) = r in
        ItemSet.add (Item(r, p)) s
      in
      match l with
      | [] -> acc
      | h::t ->
          let acc = List.fold_left add_rule_to_itemset acc (rules_by_symbol h)
          in helper t acc
    in helper (expected_tokens_of_itemset s) s
  in
  
  (* Starting itemset *)
  let starting_itemset = complete_itemset (itemset_by_token start)
  in
  
  (* Returns itemset created by application token t to itemset i *)
  let apply_token_to_itemset i t =
    let itemset_succesor : ((ItemSet.t) TokenMap.t) ItemSetMap.t =
      (* item -> token -> item *)
      let apply_token_to_item i t =
        match i with
        | Item(r, h::u) when h = t -> Some(Item(r, u))
        | _ -> None
      in
      (* item set -> token -> item set *)
      let apply_token_to_itemset i t =
        (* Inner version of application *)
        ItemSet.fold 
          (fun elt acc -> match apply_token_to_item elt t with Some(i) -> ItemSet.add i acc | None -> acc)
          i (ItemSet.empty)
      in
      (* Applies everything to itemset and do what it can *)
      (* item set -> (item_to_itemset, itemset_successor, itemset_processing_list) -> (item_to_itemset, itemset_successor, itemset_processing_list) *)
      let process_itemset is st =
        let tokens = expected_tokens_of_itemset is in
        let helper (item_to_itemset, itemset_successor, itemset_processing_list) t =
          let outset = apply_token_to_itemset is t in
          let item_from_outset = ItemSet.choose outset in
          if ItemMap.mem item_from_outset item_to_itemset then
            (* Trzeba tylko dodać powiązanie w itemset_successor *)
            let itemset_entry = if ItemSetMap.mem is itemset_successor then ItemSetMap.find is itemset_successor else TokenMap.empty in
            let itemset_successor = ItemSetMap.add is (TokenMap.add t (ItemMap.find item_from_outset item_to_itemset) itemset_entry) itemset_successor in
            (item_to_itemset, itemset_successor, itemset_processing_list)
          else
            (* Tworzymy nowy itemset, więc trzeba wszystko uaktualnić *)
            let full_outset = complete_itemset outset in
            let item_to_itemset = ItemSet.fold (fun elt acc -> if ItemMap.mem elt acc then acc else ItemMap.add elt full_outset acc) full_outset item_to_itemset in
            let itemset_entry = if ItemSetMap.mem is itemset_successor then ItemSetMap.find is itemset_successor else TokenMap.empty in
            let itemset_successor = ItemSetMap.add is (TokenMap.add t (ItemMap.find item_from_outset item_to_itemset) itemset_entry) itemset_successor in
            let itemset_processing_list = full_outset :: itemset_processing_list in
            (item_to_itemset, itemset_successor, itemset_processing_list)
        in List.fold_left helper st tokens
      in
      let item_to_itemset = ItemSet.fold (fun elt acc -> acc) starting_itemset ItemMap.empty in
      let itemset_successor = ItemSetMap.empty in
      let itemset_processing_list = [starting_itemset] in
      let rec helper (item_to_itemset, itemset_successor, itemset_processing_list) =
        match itemset_processing_list with
        | [] -> itemset_successor
        | h::t -> helper (process_itemset h (item_to_itemset, itemset_successor, t))
      in helper (item_to_itemset, itemset_successor, itemset_processing_list)
    in
    let dict = ItemSetMap.find i itemset_succesor in TokenMap.find t dict
  in
  
  (* TODO *)
  
  (starting_itemset, apply_token_to_itemset)
