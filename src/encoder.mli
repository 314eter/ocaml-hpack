open S

type t

val create : int -> t
(** [create capacity] intializes an encoder with a dynamic table with maximal
    size [capacity]. This size is an approximation of the memory usage in
    bytes. *)

module Make (C : C) (IO : IO with type 'a t = 'a C.t) : sig
  val encode_header : t -> IO.oc -> header -> unit IO.t
  (** [encode_headers decoder oc header] writes an encoded header to
      the output channel [oc] *)
end
(** The encoder functorizes over an IO monad. *)
