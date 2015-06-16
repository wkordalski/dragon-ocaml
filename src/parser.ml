module type ITEMSETMANAGER =
sig
  type tok
  type iset
  val starting : iset
  val fold : (iset -> 'a -> 'a) -> 'a -> 'a
  val iter : (iset -> unit) -> unit
  val apply : iset -> tok -> iset
end

module FirstSetMaker (T : Grammar.TOKEN) (S : Grammar.SEMANTIC)
  (G : Grammar.GRAMMAR with type tok = T.t and type sem = S.t and type rul = Rule.Make(T)(S).t) =
struct
  module R = Rule.Make(T)(S)
  module TS = Set.Make(T)
  module TM = Map.Make(T)
  
  type tok = T.t
  type tok_set = TS.t
  type 'a tok_map = 'a TM.t 
  
  let database : tok_set tok_map =
    let empty_tokens : tok_set =
      let rec process_token t (e, p) =
        if TS.mem t p then (TS.mem t e, e, p) else
        let process_rule rule (r, e, p) =
          if r then (true, e, p) else
          let q = R.production rule in
          let process_symbol s (r, e, p) =
            if r then process_token s (e, p) else (false, e, p) 
          in
          List.fold_left (fun a s -> process_symbol s a) (true, e, p) q
        in
        let q = G.rules t in
        let (r', e', p') = List.fold_left (fun a r -> process_rule r a) (false, e, p) q
        in
        if r' then (true, TS.add t e', TS.add t p') else (false, e', TS.add t p')
      in
      let (r, e, p) = process_token G.start (TS.empty, TS.empty)
      in e
    in
    let is_empty t = TS.mem t empty_tokens
    in
    let get_neighbours t = (* lista pierwszych symboli *)
      if T.is_terminal t then [] else
      if T.is_empty t then [] else
      (* T.is_nonterminal t *)
      let process_rule r a =
        let p = R.production r in
        let (e,l) = List.fold_left (fun (e, l) s -> if e then (is_empty s, s::l) else (false,l)) (true,a) p
        in if e then (T.empty)::l else l
      in List.fold_left (fun a r -> process_rule r a) [] (G.rules t)
    in
    (* Stwórz tablicę sąsiadów, przejdź ją            *)
    (* Elementy cyklu mają tą samą wartość.           *)
    (* Dwa cykle ze wspólnym elementem to jeden cykl. *)
    (* Dla nie-cykli jest już prosto.                 *)
    (* Możesz użyć find-union                         *)
    TM.empty
  
  let get (t : tok) : tok_set =
    if TM.mem t database then TM.find t database else TS.empty
  
end

module ItemSetManager (T : Grammar.TOKEN) (S : Grammar.SEMANTIC)
  (G : Grammar.GRAMMAR with type tok = T.t and type sem = S.t and type rul = Rule.Make(T)(S).t)
  :
sig

end
  =
struct
  module R = Rule.Make(T)(S)
  module I = Item.Make(T)(S)(G)
  module IS = Itemset.Make(T)(S)(G)
  module TM = Map.Make(T)
  module TS = Set.Make(T)
  module ISM = Map.Make(IS)
  
  type tok = T.t
  type sem = S.t
  type rul = R.t
  type iset = IS.t
  
  let starting = IS.starting
  let ending = IS.empty
  
  let database : IS.t TM.t ISM.t =
    (* Applies everything to itemset and do what it can *)
    (* item set -> (itemset_successor, itemset_processing_list) -> (itemset_successor, itemset_processing_list) *)
    let process_itemset is st =
      let tokens = IS.expected_tokens is in
      let helper t (itemset_successor, itemset_processing_list) =
        let full_outset = IS.apply_token is t in
        let itemset_entry = ISM.find is itemset_successor in
        let itemset_successor = ISM.add is (TM.add t full_outset itemset_entry) itemset_successor in
        if ISM.mem full_outset itemset_successor then
          (itemset_successor, itemset_processing_list)
        else
          (* Tworzymy nowy IItemSet, więc trzeba wszystko uaktualnić *)
          let itemset_successor = ISM.add full_outset TM.empty itemset_successor in
          let itemset_processing_list = full_outset :: itemset_processing_list in
          (itemset_successor, itemset_processing_list)
      in TS.fold helper tokens st
    in
    let itemset_successor = ISM.add starting TM.empty ISM.empty in
    let itemset_processing_list = [starting] in
    let rec helper (itemset_successor, itemset_processing_list) =
      match itemset_processing_list with
      | [] -> itemset_successor
      | h::t -> helper (process_itemset h (itemset_successor, t))
    in helper (itemset_successor, itemset_processing_list)
  
  
  let fold f a = ISM.fold (fun e _ a -> f e a) database a
  let iter f = ISM.iter (fun e _ -> f e) database
  let apply is t =
    if is = starting && T.equal t G.start then ending else
    let dict = ISM.find is database in TM.find t dict
end

module Make (T : Grammar.TOKEN) (S : Grammar.SEMANTIC)
  (G : Grammar.GRAMMAR with type tok = T.t and type sem = S.t and type rul = Rule.Make(T)(S).t) =
struct
  module R = Rule.Make(T)(S)
  module I = Item.Make(T)(S)(G)
  module IS = Itemset.Make(T)(S)(G)
  module TS = Set.Make(T)
  module TM = Map.Make(T)
  module IM = Map.Make(I)
  module ISM = Map.Make(IS)
  
  module ISManager = ItemSetManager(T)(S)(G)

  module ET : Grammar.TOKEN =
  struct
    type t = (IS.t * T.t * IS.t)
    
    let compare a b =
      let (s1, t1, u1) = a in
      let (s2, t2, u2) = b in
      let c = T.compare t1 t2 in
      if c <> 0 then c else
      let c = IS.compare s1 s2 in
      if c <> 0 then c else
      let c = IS.compare u1 u2 in
      if c <> 0 then c else 0
    let equal a b = (compare a b) = 0
    let is_terminal (_, t, _) = T.is_terminal t
    let is_nonterminal (_, t, _) = T.is_nonterminal t
    let is_empty (_, t, _) = T.is_empty t
    let empty = (IS.empty, T.empty, IS.empty)
    let print t = failwith "Unsupported"
  end
  module ER = Rule.Make(ET)(S)
  module ETM = Map.Make(ET)
  
  module EG
    : Grammar.GRAMMAR with type tok = ET.t and type sem = S.t and type rul = ER.t
    =
  struct
    type tok = ET.t
    type sem = S.t
    type rul = ER.t
    
    let rules t = []
    let start = ET.empty
  end
  
  type tok = T.t
  type sem = S.t
  type rul = R.t
  type itm = I.t
  
  type state = unit
  type result =
    | Shift of state
    | Reduction of state * sem
    | Acceptance of sem
  
  let print_itemset s =
  (
    IS.iter (fun i -> (I.print i; print_newline ())) s;
    print_newline()
  )
  
  let parse (s : state) (t : tok) (v : sem) : result list = []
  let forward (s : state) (t : state) : state = ()
end
(*
let parse rules start =
  
  (* IItemSet.t -> item -> extended_rule *)
  let extended_grammar_rule_from_item s i =
    let t = IItem.head i in
    let p = IItem.production i in
    let q = IItem.index i in
    let f = IItem.action i in
    let (exg, tis) = List.fold_left (fun (l, s) t -> let u = apply_token_to_itemset s t in ((s, t, u)::l, u)) ([], s) p in
    let u = apply_token_to_itemset s t in
    ((s, t, u), List.rev exg, f)
  in
   
  let add_itemset_to_extended_grammar_rules s l =
    let items = IItemSet.filter (fun x -> IItem.index x = 0) s in
    let add_item_to_extended_grammar_rules i l =
      let exr = extended_grammar_rule_from_item s i in
      let (_, t, u) = ExtendedRule.head exr in
      let lhs = (s, t, u) in
      let rhss = if ExtendedTokenMap.mem lhs l then ExtendedTokenMap.find lhs l else [] in
      ExtendedTokenMap.add lhs (exr::rhss) l
    in
    IItemSet.fold add_item_to_extended_grammar_rules items l
  in
  let extended_rules : extended_rule list ExtendedTokenMap.t =
    IItemSetMap.fold (fun s _ l -> add_itemset_to_extended_grammar_rules s l) itemset_succesor ExtendedTokenMap.empty
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
*)
