open S

type t

val create : int -> t
(** [create capacity] intializes a decoder with a dynamic table with maximal
    size [capacity]. This size is an approximation of the memory usage in
    bytes. *)

val set_capacity : t -> int -> unit
(** [set_capacity decoder capacity] sets the capacity of the dynamic table. *)

module Make (IO : IO) : sig
  val decode_headers : t -> IO.ic -> int -> header list IO.t
  (** [decode_headers decoder ic length] reads [length] bytes from the input
      channel [ic] and decodes to a list of headers. *)
end
(** The decoder functorizes over an IO monad. *)
