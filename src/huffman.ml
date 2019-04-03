open S
open Huffman_table

let encoded_length s =
  let length = String.length s in
  let rec loop bits i =
    if i < length then
      loop (bits + snd encode_table.(int_of_char s.[i])) (i + 1)
    else (bits + 7) / 8 in
  loop 0 0

let encode t s =
  let bits = ref 0 and bits_left = ref 40 in
  for i = 0 to String.length s - 1 do
    let (code, length) = encode_table.(int_of_char s.[i]) in
    bits_left := !bits_left - length;
    bits := !bits lor (code lsl !bits_left);
    while !bits_left <= 32 do
      Faraday.write_uint8 t (!bits lsr 32);
      bits := !bits lsl 8;
      bits_left := !bits_left + 8;
    done
  done;
  if !bits_left < 40 then begin
    bits := !bits lor (1 lsl !bits_left - 1);
    Faraday.write_uint8 t (!bits lsr 32);
  end

let add_output buffer = function
  | Some c -> Buffer.add_char buffer c
  | None -> ()

let get_id = function
  | Some id -> id
  | None -> raise Compression_error

let decode s =
  let length = String.length s in
  let buffer = Buffer.create length in
  let rec loop id accept i =
    if i < length then
      let input = int_of_char s.[i] in
      let index = (id lsl 4) + (input lsr 4) in
      let (state, _, output) = decode_table.(index) in
      add_output buffer output;
      let id = get_id state in
      let index = (id lsl 4) + (input land 0x0f) in
      let (state, accept, output) = decode_table.(index) in
      add_output buffer output;
      let id = get_id state in
      loop id accept (i + 1)
    else if not accept then raise Compression_error in
  loop 0 true 0;
  Buffer.contents buffer
