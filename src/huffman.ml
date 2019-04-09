open Huffman_table

exception Compression_error

let encoded_length s =
  let rec loop length bits i =
    if i < length then
      loop length (bits + snd encode_table.(int_of_char s.[i])) (i + 1)
    else (bits + 7) / 8 in
  loop (String.length s) 0 0

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

let decode s =
  let length = String.length s in
  let rec loop length buffer id accept i =
    if i < length then
      let input = int_of_char s.[i] in
      let index = (id lsl 4) + (input lsr 4) in
      let (id, _, output, c) = decode_table.(index) in
      if id < 0 then raise Compression_error;
      if output then Buffer.add_char buffer c;
      let index = (id lsl 4) + (input land 0x0f) in
      let (id, accept, output, c) = decode_table.(index) in
      if id < 0 then raise Compression_error;
      if output then Buffer.add_char buffer c;
      loop length buffer id accept (i + 1)
    else if not accept then
      raise Compression_error
    else
      Buffer.contents buffer in
  loop length (Buffer.create length) 0 true 0
