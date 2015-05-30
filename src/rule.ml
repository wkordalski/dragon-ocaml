
module type RULE =
sig
  type t
  type token
  
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val head : t -> token
  val production : t -> token list
  val action : t -> (Node.t list -> Node.t)
  val make : token -> token list -> (Node.t list -> Node.t) -> t
end

module Make(T:Token.TOKEN) =
struct
  type t = (T.t * T.t list * (Node.t list -> Node.t))
  type token = T.t
  
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
end