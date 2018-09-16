open S

type t

val create : int -> t
(** [create capacity] intializes a decoder with a dynamic table with maximal
    size [capacity]. This size is an approximation of the memory usage in
    bytes. *)

val set_capacity : t -> int -> unit
(** [set_capacity decoder capacity] sets the capacity of the dynamic table. *)

module Make (C : C) (IO : IO with type 'a t = 'a C.t) : sig
  val decode_headers : t -> IO.ic -> (header -> unit IO.t) -> (unit, exn) result IO.t
  (** [decode_headers decoder ic f] reads headers from the input
      channel [ic] and calls [f header] for each header. *)
end
(** The decoder functorizes over an IO monad. *)
