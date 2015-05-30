type node = Node.t

module TokenSet = Set.Make(Token)
module TokenMap = Map.Make(Token)

module TRule = Rule.Make(Token)

type rule = TRule.t
type item = Item of (rule * Token.t list)

module IItem =
struct
  type t = item
  let compare a b =
    let Item(x, p), Item(y, q) = a, b in
    let c = TRule.compare x y in
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

let rec list_cmp p q =
  match p, q with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | a::x, b::y when Token.equal a b -> list_cmp x y
  | _ -> false

let print_token t =
    match t with
    | Token.Terminal(x) -> print_int x
    | Token.Nonterminal(x) -> print_int x
    | Token.Empty -> print_string "<>"

let print_item i =
  let rec helper p q =
    match p with
    | [] -> ()
    | h::t when list_cmp p q -> (print_string "* "; print_token h; print_string " "; helper t [])
    | h::t -> (print_token h; print_string " "; helper t q)
  in 
  let Item(r, q) = i in
  let t = TRule.head r in
  let p = TRule.production r in
  (
    print_token t;
    print_string " -> ";
    helper p q;
    print_newline ()
  )

let print_itemset is =
(
  ItemSet.iter print_item is;
  print_newline ()
)

type extended_token = (Token.t * ItemSet.t)

module ExtendedToken =
struct
  type t = (ItemSet.t * Token.t * ItemSet.t)
  let compare a b =
    let (s1, t1, u1) = a in
    let (s2, t2, u2) = b in
    let c = Token.compare t1 t2 in
    if c <> 0 then c else
    let c = ItemSet.compare s1 s2 in
    if c <> 0 then c else
    let c = ItemSet.compare u1 u2 in
    if c <> 0 then c else 0
  let equal a b = (compare a b) = 0
  let is_terminal (s, t, u) = Token.is_terminal t
  let is_nonterminal (s, t, u) = Token.is_nonterminal t
end

module ExtendedRule = Rule.Make(ExtendedToken)
type extended_rule = ExtendedRule.t


module ExtendedTokenMap = Map.Make(ExtendedToken)

module type TOKEN =
sig
  type t
  val is_terminal : t -> bool
  val is_nonterminal : t -> bool
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module MakeFirstSet (T:Token.TOKEN) =
struct
  module TRule = Rule.Make(T)
  module TMap = Map.Make(T)
  
  let make rule_productions =
    let rec helper token acc =
      if TMap.mem token acc then acc else
      if T.is_terminal token then TMap.add token token acc else
      let prods :TRule.t list TMap.t = rule_productions token in
      acc
    in ()
end

let parse rules start =
  
  (* Token -> Rule list translation *)
  let rules_by_symbol t =
    let database : (rule list) TokenMap.t =
      let helper acc r =
        let (t, _, _) = r in
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
      | ((_, p, _) as h)::t -> helper t (ItemSet.add (Item(h, p)) acc)
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
      let add_rule_to_itemset (l,s) r =
        let (_, p, _) = r in
        let citem = Item(r, p) in
        if ItemSet.mem citem s then (l, s) else
          match p with
          | [] -> (l, ItemSet.add citem s)
          | h::t -> (h::l, ItemSet.add citem s)
      in
      match l with
      | [] -> acc
      | h::t ->
          let (l,acc) = List.fold_left add_rule_to_itemset (t,acc) (rules_by_symbol h)
          in helper l acc
    in helper (expected_tokens_of_itemset s) s
  in
  
  (* Starting and ending itemset *)
  let starting_itemset = complete_itemset (itemset_by_token start)
  and ending_itemset = ItemSet.empty
  in
  
  let itemset_succesor : ((ItemSet.t) TokenMap.t) ItemSetMap.t =
    (* item -> token -> item *)
    let apply_token_to_item i t =
      match i with
      | Item(r, h::u) when Token.equal h t -> Some(Item(r, u))
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
    (* item set -> (itemset_successor, itemset_processing_list) -> (itemset_successor, itemset_processing_list) *)
    let process_itemset is st =
      let tokens = expected_tokens_of_itemset is in
      let helper (itemset_successor, itemset_processing_list) t =
        let outset = apply_token_to_itemset is t in
        let full_outset = complete_itemset outset in
        let itemset_entry = ItemSetMap.find is itemset_successor in
        let itemset_successor = ItemSetMap.add is (TokenMap.add t full_outset itemset_entry) itemset_successor in
        if ItemSetMap.mem full_outset itemset_successor then
          (itemset_successor, itemset_processing_list)
        else
          (* Tworzymy nowy itemset, więc trzeba wszystko uaktualnić *)
          let itemset_successor = ItemSetMap.add full_outset TokenMap.empty itemset_successor in
          let itemset_processing_list = full_outset :: itemset_processing_list in
          (itemset_successor, itemset_processing_list)
      in List.fold_left helper st tokens
    in
    let itemset_successor = ItemSetMap.add starting_itemset TokenMap.empty ItemSetMap.empty in
    let itemset_processing_list = [starting_itemset] in
    let rec helper (itemset_successor, itemset_processing_list) =
      match itemset_processing_list with
      | [] -> itemset_successor
      | h::t -> helper (process_itemset h (itemset_successor, t))
    in helper (itemset_successor, itemset_processing_list)
  in
  (* Returns itemset created by application token t to itemset i *)
  let apply_token_to_itemset i t =
    if i = starting_itemset && Token.equal t start then ending_itemset else
    let dict = ItemSetMap.find i itemset_succesor in TokenMap.find t dict
  in
  
  (* ItemSet.t -> item -> extended_rule *)
  let extended_grammar_rule_from_item s i =
    let Item((t,p,f),q) = i in
    let (exg, tis) = List.fold_left (fun (l, s) t -> let u = apply_token_to_itemset s t in ((s, t, u)::l, u)) ([], s) p in
    let u = apply_token_to_itemset s t in
    ((s, t, u), List.rev exg, f)
  in
   
  let add_itemset_to_extended_grammar_rules s l =
    let items = ItemSet.filter (function Item((_, p, _), q) -> list_cmp p q) s in
    let add_item_to_extended_grammar_rules i l =
      let exr = extended_grammar_rule_from_item s i in
      let (_, t, u) = ExtendedRule.head exr in
      let lhs = (s, t, u) in
      let rhss = if ExtendedTokenMap.mem lhs l then ExtendedTokenMap.find lhs l else [] in
      ExtendedTokenMap.add lhs (exr::rhss) l
    in
    ItemSet.fold add_item_to_extended_grammar_rules items l
  in
  let extended_rules : extended_rule list ExtendedTokenMap.t =
    ItemSetMap.fold (fun s _ l -> add_itemset_to_extended_grammar_rules s l) itemset_succesor ExtendedTokenMap.empty
  in
  
  let make_first_set is_terminal extract_nonterminal rule_productions empty_set_map find_in_map is_map_member add_to_map =
    let rec helper token acc =
      if is_map_member token acc then acc else
      if is_terminal token then add_to_map token token acc else
      let prods = rule_productions token in
      acc
    in ()
  in
  ()
 


let xxx = 0

