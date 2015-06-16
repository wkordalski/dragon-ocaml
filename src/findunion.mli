type 'a t

val make : 'a -> 'a t
val get : 'a t -> 'a

val find : 'a t -> 'a t
val union : 'a t -> 'a t -> unit