module Make (T : Grammar.TOKEN) (S : Grammar.SEMANTIC)
  (G : Grammar.GRAMMAR with type tok = T.t and type sem = S.t and type rul = Rule.Make(T)(S).t)
  : (Grammar.ITEM with type tok := G.tok and type sem := G.sem and type rul := G.rul) =
struct
  module R = Rule.Make(T)(S)
  
  type tok = T.t
  type sem = S.t
  type rul = R.t
  type t = (tok * tok list * int * (sem list -> sem))
  
  let compare (a : t) (b : t) : int =
    let (t1, p1, q1, f1) = a in
    let (t2, p2, q2, f2) = b in
    let c = T.compare t1 t2 in
    if c <> 0 then c else
    let c = Pervasives.compare q1 q2 in
    if c <> 0 then c else
    let c = List.fold_left2 (fun a x y -> if a <> 0 then a else T.compare x y) 0 p1 p2 in
    if c <> 0 then c else 0
  let equal a b = (compare a b) = 0
  let head (t,_,_,_) = t
  let production (_,p,_,_) = p
  let action (_,_,_,f) = f
  let expected (_,p,q,_) = try Some(List.nth p q) with Failure("nth") -> None
  let index (_,_,q,_) = q
  let make r = (R.head r, R.production r, 0, R.action r)
  let apply i t =
    match expected i with
    | Some(u) when T.equal t u -> let (t, p, q, f) = i in Some(t, p, q+1, f)
    | _ -> None 
  let print (t,p,q,_) =
    let helper2 p =
      List.iter (fun t -> (print_string " "; T.print t)) p
    in
    let rec helper p q =
      if q == 0 then (print_string " *"; helper2 p)
      else match p with []->() | h::t -> (print_string " "; T.print h; helper t (q-1))
    in
    (
      T.print t;
      print_string " ->";
      helper p q
    )
end