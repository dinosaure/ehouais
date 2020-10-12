type buf = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
type t

exception Invalid_bit

val allocator : int -> buf
val make : allocator:(int -> buf) -> t
val each_bit : t -> (int -> unit) -> unit
val set : t -> int -> unit
val xor : t -> t -> t -> unit
val pp : t Fmt.t
