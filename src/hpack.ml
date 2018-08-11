include S

module Decoder = Decoder
module Encoder = Encoder

module Decoder_lwt = Decoder.Make (Io_lwt)
module Decoder_unix = Decoder.Make (Io_unix)

module Encoder_lwt = Encoder.Make (Io_lwt)
module Encoder_unix = Encoder.Make (Io_unix)
