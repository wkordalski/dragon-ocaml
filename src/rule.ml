module Make(T: Grammar.TOKEN) (S: Grammar.SEMANTIC) : (Grammar.RULE with type tok := T.t and type sem := S.t) =
struct
  module T = T
  module S = S
  
  type tok = T.t
  type sem = S.t
  type t = (tok * tok list * (sem list -> sem))
  
  let compare a b =
    let (x, k, f), (y, l, g) = a, b in
    let c = T.compare x y in
    if c <> 0 then c else
    let c = List.fold_left2 (fun a x y -> if a <> 0 then a else T.compare x y) 0 k l in
    if c <> 0 then c else 0
  let equal a b = (compare a b) = 0
  let head (t,_,_) = t
  let production (_, p,_) = p
  let action (_,_,f) = f
  let make t l f = (t, l, f)
  let print (t,p,_) =
  (
    T.print t;
    print_string " ->";
    List.iter (fun t -> (print_string " "; T.print t)) p
  )
end