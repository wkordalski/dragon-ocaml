module type SEMANTIC =
sig
  type t
end

module type TOKEN =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_terminal : t -> bool
  val is_nonterminal : t -> bool
  val is_empty : t -> bool
  val is_ending : t -> bool
  val empty : t
  val ending : t
  val print : t -> unit
end

module type RULE =
sig  
  type t
  type tok
  type sem
  
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val head : t -> tok
  val production : t -> tok list
  val action : t -> (sem list -> sem)
  val make : tok -> tok list -> (sem list -> sem) -> t
  val print : t -> unit
end

module type GRAMMAR =
sig  
  type tok
  type sem
  type rul
  
  val rules : tok -> rul list
  val start : tok
  val tokens : tok list
end

module type ITEM =
sig  
  type t
  type tok
  type sem
  type rul
  
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val head : t -> tok
  val production : t -> tok list
  val action : t -> (sem list -> sem)
  val expected : t -> tok option
  val index : t -> int
  val make : rul -> t
  val apply : t -> tok -> t option
  val print : t -> unit
end

module type ITEMSET =
sig
  type tok
  type tok_set
  type itm
  
  include Set.S with type elt = itm 
    
  val starting : t
  val expected_tokens : t -> tok_set
  val complete : t -> t
  val apply_token : t -> tok -> t
end
