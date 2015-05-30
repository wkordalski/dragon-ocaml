
module TRule :
sig
  type t
  type token = Token.t
  
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val head : t -> token
  val production : t -> token list
  val action : t -> (Node.t list -> Node.t)
  val make : token -> token list -> (Node.t list -> Node.t) -> t
end

type item = Item of (TRule.t * Token.t list)

val parse : TRule.t list -> Token.t -> unit
