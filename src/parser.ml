(* Learned LALR(1) parsing algorithm from here: *)
(* http://web.cs.dal.ca/~sjackson/lalr1.html    *)

module type ITEMSETMANAGER =
sig
  type tok
  type iset
  val starting : iset
  val fold : (iset -> 'a -> 'a) -> 'a -> 'a
  val iter : (iset -> unit) -> unit
  val apply : iset -> tok -> iset
end

module type FIRSTSETMAKER =
sig
  type tok
  type tok_set
  val get : tok -> tok_set
end

module ItemSetManager (T : Grammar.TOKEN) (S : Grammar.SEMANTIC)
  (G : Grammar.GRAMMAR with type tok = T.t and type sem = S.t and type rul = Rule.Make(T)(S).t)
  : ITEMSETMANAGER with type tok = T.t and type iset = Itemset.Make(T)(S)(G).t
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
    if IS.equal is starting && T.equal t G.start then ending else
    let dict = ISM.find is database in TM.find t dict
end

module FirstSetMaker (T : Grammar.TOKEN) (S : Grammar.SEMANTIC)
  (G : Grammar.GRAMMAR with type tok = T.t and type sem = S.t and type rul = Rule.Make(T)(S).t)
  : FIRSTSETMAKER with type tok = T.t and type tok_set = Set.Make(T).t
  =
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
    let get_neighbours (t : tok) : tok list =
      if T.is_terminal t then [] else
      if T.is_empty t then [] else
      (* T.is_nonterminal t *)
      let process_rule r a =
        let p = R.production r in
        let (e,l) = List.fold_left (fun (e, l) s -> if e then (is_empty s, s::l) else (false,l)) (true,a) p
        in if e then (T.empty)::l else l
      in List.fold_left (fun a r -> process_rule r a) [] (G.rules t)
    in
    let node_to_representative : tok Findunion.t TM.t = 
	    let rec helper (t : tok) (a : tok Findunion.t TM.t) : tok Findunion.t TM.t =
	      if TM.mem t a then a else
	      let rule_processor r a =
	        List.fold_left (fun a e -> helper e a) a (R.production r)
	      in
	      let rr = G.rules t in
	      let a = TM.add t (Findunion.make t) a in
	      List.fold_left (fun a e -> rule_processor e a) a rr
	    in
      helper G.start TM.empty
    in
    let get_representative (t : tok) : tok =
      if T.is_empty t then t else
      let tfu = TM.find t node_to_representative in
      let rfu = Findunion.find tfu in
      let r = Findunion.get rfu in
      r
    in 
    let neighbour_table : TS.t TM.t=
      let rec helper (t : tok) (s, l : TS.t * tok list) (v, a : TS.t * TS.t TM.t) : (TS.t * TS.t TM.t) =
        let unify (l : tok list) (a : TS.t TM.t) =
          let rec helper (l : tok list) (a : TS.t TM.t) =
            match l with
            | [] -> assert false
            | h::_ when h = t -> a
            | h::r ->
              let tt = TM.find t node_to_representative in
              let hh = TM.find t node_to_representative in
              let _ = Findunion.union hh tt in
              TM.add t (TS.union (TM.find t a) (TM.find h a)) a
          in helper l a
        in
        if TS.mem t s then (TS.add t v, unify l a) else
        if TS.mem t v then (v, a) else
        let ts = TS.of_list (get_neighbours t) in
        let a = TM.add t ts a in
        let v = TS.add t v in
        let rep = get_representative t in
        let a = TM.add rep (TS.union (TM.find rep a) ts) a in
        let (v,a) = TS.fold (fun e a -> helper e (TS.add t s, t::l) a) ts (v,a) in 
        (v, a)
      in
      let (_, a) = helper G.start (TS.empty, []) (TS.empty, TM.empty)
      in a
    in
    
    let rec helper (t : tok) (a : TS.t TM.t) : TS.t TM.t =
      if TM.mem t a then a else
      if T.is_empty t then TM.add t TS.empty a else
      if T.is_terminal t then TM.add t (TS.add t TS.empty) a else
      let r = get_representative t in
      if T.equal t r then
        let nbset = TM.find t neighbour_table in
        let nbset = TS.filter (fun e -> not (T.equal (get_representative e) r)) nbset in
        let a = TS.fold (fun e a -> helper e a) nbset a in
        let fs = TS.fold (fun e ac -> TS.union (TM.find e a) ac) nbset TS.empty in
        TM.add t fs a
      else
        let a = helper r a in
        TM.add t (TM.find r a) a
    in
    let res = List.fold_left (fun a e -> helper e a) TM.empty G.tokens in
    let res = TS.fold (fun e a -> let s = TM.find e a in TM.add e (TS.add T.empty s) a) empty_tokens res in
    let _ = TM.iter (fun e v -> T.print e; print_string ": "; (TS.iter (fun t -> (T.print t; print_string " ")) v); print_newline ()) res in
    res
      
  let get (t : tok) : tok_set =
    if TM.mem t database then TM.find t database else TS.empty
  
end

module FollowSetMaker (T : Grammar.TOKEN) (S : Grammar.SEMANTIC)
  (G : Grammar.GRAMMAR with type tok = T.t and type sem = S.t and type rul = Rule.Make(T)(S).t)
  (F : FIRSTSETMAKER with type tok = T.t and type tok_set = Set.Make(T).t)
  =
struct
  module TS = Set.Make(T)
  module TM = Map.Make(T)
  
  type tok = T.t
  type tok_set = TS.t
  
  let database : TS.t TM.t =
    let rec helper (a : TS.t TM.t) : TS.t TM.t = a
    in
    helper (TM.add G.start (TS.add T.ending TS.empty) TM.empty)
  
  let get (t : tok) : tok_set =
    if TM.mem t database then TM.find t database else TS.empty
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
  module FirstSet = FirstSetMaker(T)(S)(G)

  module ET =
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
    let is_ending (_, t, _) = T.is_ending t
    let empty = (IS.empty, T.empty, IS.empty)
    let ending = (IS.empty, T.ending, IS.empty)
    let print t = failwith "Unsupported"
    let make (s : IS.t) (t : T.t) = (s, t, ISManager.apply s t)
    let source (s, _, _) = s
    let destination (_,_,d) = d
    let token (_,t,_) = t
  end
  module ER = Rule.Make(ET)(S)
  module ETS = Set.Make(ET)
  module ETM = Map.Make(ET)
  
  module EG
    : Grammar.GRAMMAR with type tok = ET.t and type sem = S.t and type rul = ER.t
    =
  struct
    type tok = ET.t
    type sem = S.t
    type rul = ER.t
        
    type itm = I.t
    type its = IS.t
    
    let rule_from_item (s : its) (i : itm) : rul =
      assert (I.index i = 0);
      let t = I.head i in
      let p = I.production i in
      let f = I.action i in
      let (exg, tis) = List.fold_left (fun (l, s) t -> let rs = ET.make s t in (rs::l, ET.destination rs)) ([], s) p in
      ER.make (ET.make s t) (List.rev exg) f
    
    let process_itemset (s : its) (l : rul list ETM.t) : rul list ETM.t =
      let items = IS.filter (fun x -> I.index x = 0) s in
      let add_item_to_extended_grammar_rules i l =
        let exr = rule_from_item s i in
        let head = ER.head exr in
        let rhs = if ETM.mem head l then ETM.find head l else [] in
        ETM.add head (exr::rhs) l
      in
      IS.fold add_item_to_extended_grammar_rules items l
      
    let extended_rules : rul list ETM.t = ISManager.fold (fun s l -> process_itemset s l) ETM.empty
    
    let rules t = try ETM.find t extended_rules with Not_found -> []
    let start = ET.make ISManager.starting G.start
    let tokens = 
      let parse_rule (r : rul) (a : ETS.t) : ETS.t =
        let a = ETS.add (ER.head r) a in
        let a = List.fold_left (fun a t -> ETS.add t a) a (ER.production r) in
        a
      in
      let tokset =
        ETM.fold (fun _ v a -> List.fold_left (fun a r -> parse_rule r a) a v) extended_rules ETS.empty
      in
      ETS.fold (fun e a -> e::a) tokset []
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

