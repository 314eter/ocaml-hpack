exception Compression_error
exception Decoding_error

type header = {
  name : string;
  value : string;
  never_index : bool;
}

module type IO = sig
  type ic
  type oc

  val read_byte : ic -> int Lwt.t
  val write_byte : oc -> int -> unit Lwt.t

  val read_string : ic -> int -> string Lwt.t
  val write_string : oc -> string -> unit Lwt.t
end
