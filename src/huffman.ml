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
  let rec loop length buffer id x i =
    if i < length then
      let input = int_of_char s.[i] in
      let index = (id lsl 4) + (input lsr 4) in
      let x = decode_table.(index) in
      let id = x lsr 10 in
      if id > 255 then raise Compression_error;
      if x land 0b100000000 != 0 then Buffer.add_char buffer (char_of_int (x land 0b11111111));
      let index = (id lsl 4) + (input land 0x0f) in
      let x = decode_table.(index) in
      let id = x lsr 10 in
      if id > 255 then raise Compression_error;
      if x land 0b100000000 != 0 then Buffer.add_char buffer (char_of_int (x land 0b11111111));
      loop length buffer id x (i + 1)
    else if x land 0b1000000000 = 0 then
      raise Compression_error
    else Buffer.contents buffer in
  let length = String.length s in
  loop length (Buffer.create (length + length lsr 2)) 0 0b1000000000 0
