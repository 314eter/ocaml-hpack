open Hpack

module C = struct
  type +'a t = 'a

  let bind x f = f x
  let map f x = f x
  let return x = x
end

module Make_decoder (IO : IO with type 'a t = 'a) = Decoder.Make (C) (IO)
module Make_encoder (IO : IO with type 'a t = 'a) = Encoder.Make (C) (IO)
