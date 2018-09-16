open Hpack

module Make_decoder (IO : IO with type 'a t = 'a Lwt.t) = Decoder.Make (Lwt) (IO)
module Make_encoder (IO : IO with type 'a t = 'a Lwt.t) = Encoder.Make (Lwt) (IO)
