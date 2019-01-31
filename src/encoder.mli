open S

type t

val create : int -> t
(** [create capacity] intializes an encoder with a dynamic table with maximal
    size [capacity]. This size is an approximation of the memory usage in
    bytes. *)

val encode_header : t -> Faraday.t -> header -> unit
