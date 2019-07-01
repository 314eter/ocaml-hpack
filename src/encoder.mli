type t

val create : ?max_size:int -> unit -> t
(** [create ~max_size ()] intializes an encoder with a dynamic table with
    maximum size [max_size]. The default is 4096. The chosen size must be lower
    than or equal to the maximum set by the protocol. *)

val encode_header : t -> Faraday.t -> Header.t -> unit
(** [encode_header encoder t header] writes the encoding of [header] to [t] *)

val change_table_size : t -> int -> unit
(** [change_table_size encoder max_size] changes the maximum size of the dynamic
    table. This should only occur between header blocks, and is signaled in a
    dynamic table size update before the next encoded header. The chosen size
    must stay lower than or equal to the maximum set by the protocol. *)
