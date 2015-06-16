module Make (T : Grammar.TOKEN) (S : Grammar.SEMANTIC)
  (G: Grammar.GRAMMAR with type tok = T.t and type sem = S.t and type rul = Rule.Make(T)(S).t)
  : (Grammar.ITEMSET with type tok := T.t and type tok_set := Set.Make(T).t and type itm = Item.Make(T)(S)(G).t)=
struct
  module I = Item.Make(T)(S)(G)
  module R = Rule.Make(T)(S)
  module TSet = Set.Make(T)
  include Set.Make(I)
  
  type tok = T.t
  type tok_set = TSet.t
  type itm = I.t
  
  (* TODO *)
  let starting = List.fold_left (fun a r -> add (I.make r) a) empty (G.rules G.start)
  
  let expected_tokens s =
    let process_item i acc =
      match I.expected i with
      | None-> acc
      | Some(h) -> TSet.add h acc
    in
    fold process_item s TSet.empty
  
  let complete s =
    (* items list to fulfill -> output items -> output items fulfilled *)
    let rec helper l acc =
      (* ItemSet -> Rule -> ItemSet *)
      let add_rule_to_itemset (l,s) r =
        let citem = I.make r in
        if mem citem s then (l, s) else
          match I.production citem with
          | [] -> (l, add citem s)
          | h::t -> (TSet.add h l, add citem s)
      in
      if TSet.is_empty l then acc else
      let h = TSet.choose l in
      let t = TSet.remove h l in
      let (l, acc) = List.fold_left add_rule_to_itemset (t,acc) (G.rules h) in
      helper l acc
    in helper (expected_tokens s) s
    
  let apply_token s t =
    let outset = fold (fun elt acc -> match I.apply elt t with Some(i) -> add i acc | None -> acc) s empty
    in complete outset
end