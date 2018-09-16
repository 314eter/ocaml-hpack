open Async_kernel
open Hpack

module C = struct
  type +'a t = 'a Deferred.t

  let bind x f = Deferred.bind x ~f
  let map f x = Deferred.map x ~f
  let return = Deferred.return
end

module Make_decoder (IO : IO with type 'a t = 'a Deferred.t) = Decoder.Make (C) (IO)
module Make_encoder (IO : IO with type 'a t = 'a Deferred.t) = Encoder.Make (C) (IO)
