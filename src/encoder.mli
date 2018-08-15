open S

type t

val create : int -> t
(** [create capacity] intializes an encoder with a dynamic table with maximal
    size [capacity]. This size is an approximation of the memory usage in
    bytes. *)

module Make (IO : IO) : sig
  val encode_headers : t -> IO.oc -> header list -> unit Lwt.t
  (** [encode_headers decoder oc headers] writes an encoded list of headers to
      the output channel [oc] *)
end
(** The encoder functorizes over an IO monad. *)
