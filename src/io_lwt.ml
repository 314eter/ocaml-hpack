let char_of_int = Char.unsafe_chr

type +'a t = 'a Lwt.t

let bind = Lwt.bind
let map = Lwt.map
let return = Lwt.return

type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

let read_byte ic =
  Lwt.map int_of_char (Lwt_io.read_char ic)

let write_byte oc b =
  Lwt_io.write_char oc (char_of_int b)

let read_string ic length =
  let buffer = Bytes.create length in
  Lwt.map
    (fun () -> Bytes.unsafe_to_string buffer)
    (Lwt_io.read_into_exactly ic buffer 0 length)

let write_string = Lwt_io.write
