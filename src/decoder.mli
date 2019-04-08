open S

type t

val create : ?max_size_limit:int -> unit -> t
(** [create ~max_size_limit ()] intializes a decoder with a dynamic table with
    maximum size [max_size_limit]. The default is 4096. Each different
    should be communicated to the encoder by the protocol. *)

val header : t -> header Angstrom.t
(** A parser for one header *)

val headers : t -> header list Angstrom.t
(** A parser for one header block *)

val change_table_size_limit : t -> int -> unit
(** [change_table_size_limit decoder max_size_limit] limits the maximum size of
    the dynamic table. This limit should be communicated to the encoder by the
    protocol, and can only change between header blocks. *)
