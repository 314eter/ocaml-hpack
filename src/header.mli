type t = private {
  name : string;
  value : string;
  never_index : bool;
}

val make : ?never_index:bool -> string -> string -> t
(** [make name value] creates a new header field. The optional argument
    [never_index] can be used for sensitive values that should not be kept in
    memory.
*)

val name : t -> string
(** Extracts the name of a header field. *)

val value : t -> string
(** Extracts the value of a header field. *)

val never_index : t -> bool
(** [never_index header] is true if this header field will never be indexed. *)

val size : t -> int
(** Returns the size of a header field, which is the length of the name and
    value in octets plus an overhead of 32 octets. *)
