type t = private {
  name : string;
  value : string;
  never_index : bool;
}

val make : ?never_index:bool -> string -> string -> t

val name : t -> string

val value : t -> string

val never_index : t -> bool
