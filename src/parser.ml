(* Learned LALR(1) parsing algorithm from here:     *)
(* http://web.cs.dal.ca/~sjackson/lalr1.html        *)
(* Not learned but it has usefull information.      *)
(* This file contains the most simple GLR algorithm *)

module ItemSetManager (T : Grammar.TOKEN) (S : Grammar.SEMANTIC)
  (G : Grammar.GRAMMAR with type tok = T.t and type sem = S.t and type rul = Rule.Make(T)(S).t)
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

  (* An augumenting rule S' -> S$ where S is starting rule *)
  let augumenter = R.make T.root [G.start; T.ending] (fun l -> List.hd l)
  (* Starting ItemSet *)
  let starting = IS.closure (IS.add (I.make augumenter) IS.empty)
  (* ItemSet after all parsing *)
  let ending =
    let aug0 = I.make augumenter in
    let aug1 = match I.apply aug0 G.start with Some(x) -> x | None -> assert false in
    let aug2 = match I.apply aug1 T.ending with Some(x) -> x | None -> assert false in
    IS.add aug2 IS.empty

  (* Applies everything to itemset and do what it can *)
  (* item set -> (itemset_successor, itemset_processing_list) -> (itemset_successor, itemset_processing_list) *)
  let process_itemset is st =
    let tokens = IS.expected_tokens is in
    let helper t (itemset_successor, itemset_processing_list) =
      (* Itemset got by application of token t *)
      let full_outset = IS.apply_token is t in
      (* Update successors of is: add t -> full_outset *)
      let itemset_entry = ISM.find is itemset_successor in
      let itemset_successor = ISM.add is (TM.add t full_outset itemset_entry) itemset_successor in
      (* Check if we need to create a new itemset from full_outset *)
      if ISM.mem full_outset itemset_successor then
        (itemset_successor, itemset_processing_list)
      else
        (* We are creating new itemset so add it to processing list *)
        let itemset_successor = ISM.add full_outset TM.empty itemset_successor in
        let itemset_processing_list = full_outset :: itemset_processing_list in
        (itemset_successor, itemset_processing_list)
    in TS.fold helper tokens st

  (* map: itemset -> token -> itemset *)
  let database : IS.t TM.t ISM.t =
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

  type tok = T.t
  type sem = S.t
  type rul = R.t
  type itm = I.t
  type its = ISManager.iset

  type state = ( its * sem ) list
  type result =
    | Result of state
    | Acceptance of sem

  let print_itemset s =
  (
    IS.iter (fun i -> (I.print i; print_newline ())) s;
    print_newline()
  )

  let print _ = ISManager.iter print_itemset

  let initial : state = []
  (* Does possible reductions, and finally proper shift. Returns possible result list. *)
  let parse (s : state) (t : tok) (v : sem) : result list =
    let get_iset (s : state) =
      match s with
      | [] -> ISManager.starting
      | (h, _)::t -> h
    in

    let is_reducable (i : itm) : bool =
      let prodlen = List.length (I.production i) in
      I.index i = prodlen && prodlen > 0
    in

    let reduce_with_rule (i : itm) (s : state) : state =
      let prodlen = List.length (I.production i) in
      let rec transfer n (a,b) =
        if n = 0 then (a, b) else
        if n < 0 then let (a, b) = transfer n (b, a) in (b, a) else
        let v = List.hd a in
        transfer (n-1) (List.tl a, v::b)
      in
      let act = I.action i in
      let (rs, ag) = transfer prodlen (s, []) in
      let (_, ags) = List.split ag in
      let av = act ags in
      let at = I.head i in
      let is = get_iset rs in
      let dis = ISManager.apply is at in
      (dis,av)::rs
    in

    let try_shift (s : state) (t : tok) (v : sem) (a : result list) : result list =
      let is = get_iset s in
      try
        let dis = ISManager.apply is t in
        if IS.compare ISManager.ending dis = 0 then
          let (_,v) = List.hd s in Acceptance(v)::a
        else
          Result((dis,v)::s)::a
      with Not_found -> a
    in

    let rec helper (s : state) (a : result list) : result list =
      (* 1. Check we can shift *)
      let a = try_shift s t v a in
      (* 2. For every possible reduction call helper with reduced state *)
      let is = get_iset s in
      IS.fold (fun e a -> helper (reduce_with_rule e s) a) (IS.filter is_reducable is) a
    in

    helper s []

  let parse_stream (s : sem Stream.t) (f : sem -> tok) : sem list =
    let rec helper s (t, a : state list * sem list) : state list * sem list =
      match Stream.peek s with
      | None -> (t, a)
      | Some(v) ->
          let _ = Stream.junk s in
          let w = f v in
          let (t, a) = List.fold_left
          (fun (q,a) e ->
            let rs = parse e w v in
            List.fold_left (fun (q,a) e -> match e with Result(s)->(s::q, a) | Acceptance(s)->(q,s::a)) (q,a) rs
          ) ([], a) t
          in
          helper s (t, a)
    in
    let (_, a) = helper s ([initial],[])
    in
    a

end
