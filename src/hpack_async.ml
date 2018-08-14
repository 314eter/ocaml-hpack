open Async_kernel
open Async_unix

module IO = struct
  let char_of_int = Char.unsafe_chr

  type +'a t = 'a Deferred.t

  let bind x f = Deferred.bind x ~f
  let map f x = Deferred.map x ~f
  let return = Deferred.return

  type ic = Reader.t
  type oc = Writer.t

  let read_byte ic =
    Reader.read_char ic >>| function
    | `Ok c -> int_of_char c
    | `Eof -> assert false

  let write_byte oc b =
    Deferred.return (Writer.write_byte oc b)

  let read_string ic length =
    let buffer = Bytes.create length in
    Reader.really_read ic buffer ~len:length >>| function
    | `Ok -> Bytes.unsafe_to_string buffer
    | `Eof _ -> assert false

  let write_string oc s =
    Deferred.return (Writer.write oc s)
end

module Decoder = Hpack.Decoder.Make (IO)
module Encoder = Hpack.Encoder.Make (IO)
