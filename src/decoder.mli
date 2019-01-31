open S

type t

val create : int -> t
(** [create capacity] intializes a decoder with a dynamic table with maximal
    size [capacity]. This size is an approximation of the memory usage in
    bytes. *)

val set_capacity : t -> int -> unit
(** [set_capacity decoder capacity] sets the capacity of the dynamic table. *)

val header : t -> header Angstrom.t

val headers : t -> header list Angstrom.t
