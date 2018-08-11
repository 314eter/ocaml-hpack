let char_of_int = Char.unsafe_chr

type +'a t = 'a

let bind x f = f x
let map f x = f x
let return x = x

type ic = in_channel
type oc = out_channel

let read_byte = input_byte

let write_byte = output_byte

let read_string = really_input_string

let write_string = output_string
