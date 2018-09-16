exception Compression_error
exception Decoding_error

type header = {
  name : string;
  value : string;
  never_index : bool;
}

module type C = sig
  type +'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type IO = sig
  type +'a t

  type ic
  type oc

  val read_byte : ic -> int option t
  val write_byte : oc -> int -> unit t

  val read_string : ic -> int -> string option t
  val write_string : oc -> string -> unit t
end
