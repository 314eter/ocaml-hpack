open S

type t = {
  table : Dynamic_table.t;
  max_capacity : int;
}

let create max_capacity = {
  table = Dynamic_table.create max_capacity;
  max_capacity = max_capacity;
}

let set_capacity {table; max_capacity} capacity =
  if capacity > max_capacity then
    raise Decoding_error
  else
    Dynamic_table.set_capacity table capacity

open Angstrom

module Let_syntax = struct
  let bind p ~f = p >>= f
  let map p ~f = p >>| f
end

let any_int prefix prefix_length =
  let max_prefix = 1 lsl prefix_length - 1 in
  let i = prefix land max_prefix in
  if i < max_prefix then
    return i
  else
    let rec loop i k =
      let%bind b = any_uint8 in
      let i = i + (b land 127) lsl k in
      if b >= 128 then
        loop i (k + 7)
      else return i in
    loop i 0

let any_string =
  let%bind b = any_uint8 in
  let%bind length = any_int b 7 in
  let%bind s = take length in
  if b < 128 then return s
  else match Huffman.decode s with
    | s -> return s
    | exception Compression_error -> fail "compression error"

let get_indexed_field table index =
  if index = 0 then
    raise Decoding_error
  else if index <= Static_table.size then
    Static_table.table.(index - 1)
  else
    Dynamic_table.get table (index - Static_table.size - 1)

let header_field table prefix prefix_length =
  let%bind index = any_int prefix prefix_length in
  let%bind name =
    if index = 0 then any_string
    else match get_indexed_field table index with
      | name, _ -> return name
      | exception Decoding_error -> fail "decoding error" in
  let%map value = any_string in
  (name, value)

let rec header ({table; _} as decoder) =
  let%bind b = any_uint8 in
  if b >= 128 then
    let%bind index = any_int b 7 in
    match get_indexed_field table index with
    | name, value -> return {name; value; never_index = false}
    | exception Decoding_error -> fail "decoding error"
  else if b >= 64 then
    let%bind (name, value) = header_field table b 6 in
    Dynamic_table.add table (name, value);
    return {name; value; never_index = false}
  else if b < 32 then
    let%bind (name, value) = header_field table b 4 in
    return {name; value; never_index = false}
  else
    let%bind capacity = any_int b 5 in
    match set_capacity decoder capacity with
    | () -> header decoder
    | exception Decoding_error -> fail "decoding error"

let headers t = many (header t)
