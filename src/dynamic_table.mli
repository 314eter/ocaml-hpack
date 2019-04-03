type t

val create : ?evict_callback:(string * string -> unit) -> int -> t

val size : t -> int

val get : t -> int -> string * string

val add : t -> string * string -> unit

val set_capacity : t -> int -> unit
