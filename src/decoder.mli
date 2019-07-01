type t

val create : ?max_size_limit:int -> ?max_field_size:int -> unit -> t
(** [create ~max_size_limit ()] intializes a decoder with a dynamic table with
    maximum size [max_size_limit]. The default is 4096. Each other value should
    be communicated to the encoder by the protocol.

    [max_field_size] is the maximum allowed size in bytes of one name or value
    field. The default is 4096.
*)

val header : t -> Header.t Angstrom.t
(** A parser for one header *)

val headers : t -> Header.t list Angstrom.t
(** A parser for one header block *)

val change_table_size_limit : t -> int -> unit
(** [change_table_size_limit decoder max_size_limit] limits the maximum size of
    the dynamic table. This limit should be communicated to the encoder by the
    protocol, and can only change between header blocks. *)
