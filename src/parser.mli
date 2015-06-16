
module TRule :
sig
  type t
  type tok = Token.t
  
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val head : t -> tok
  val production : t -> tok list
  val action : t -> (Node.t list -> Node.t)
  val make : tok -> tok list -> (Node.t list -> Node.t) -> t
  val print : t -> unit
end

val parse : TRule.t list -> Token.t -> unit
