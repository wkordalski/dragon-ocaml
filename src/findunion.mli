type 'a t

val make : 'a -> 'a t
val get : 'a t -> 'a

val find : 'a t -> 'a t

(* new representative is second argument's representative *)
val union : 'a t -> 'a t -> unit
