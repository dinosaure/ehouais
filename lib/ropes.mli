type t

val empty : t
val length : t -> int
val singleton : int -> t
val of_array : int array -> t
val append : t -> t -> t
val get : t -> int -> int
val set : t -> int -> int -> unit
val blit : t -> int -> int array -> int -> int -> unit

module Cursor : sig
  type cursor

  val make : t -> int -> cursor
  val to_ropes : cursor -> t
  val get : cursor -> int
end
