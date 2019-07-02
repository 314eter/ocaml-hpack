type t = {
  table : Dynamic_table.t;
  mutable max_size_limit : int;
  max_field_size : int;
}

exception Invalid_index

let create ?(max_size_limit=4096) ?(max_field_size=4096) () =
  if max_size_limit < 0 || max_field_size < 0 then
    raise (Invalid_argument "Decoder.create");
  {table = Dynamic_table.create max_size_limit; max_size_limit; max_field_size}

open Angstrom

let ( let* ) p f = p >>= f
let ( let+ ) p f = p >>| f

let any_int prefix prefix_length =
  let max_prefix = 1 lsl prefix_length - 1 in
  let i = prefix land max_prefix in
  if i < max_prefix then
    return i
  else
    let rec loop i k =
      if k <= 49 then
        let* b = any_uint8 in
        let i = i + (b land 127) lsl k in
        if b >= 128 then
          loop i (k + 7)
        else return i
      else fail "integer overflow" in
    loop i 0

let any_string max_size =
  let* b = any_uint8 in
  let* length = any_int b 7 in
  if length <= max_size then
    let* s = take length in
    if b < 128 then return s
    else match Huffman.decode s with
      | s -> return s
      | exception Huffman.Compression_error -> fail "compression error"
  else fail "header size too large"

let get_indexed_field table index =
  if index = 0 then
    raise Invalid_index
  else if index <= Static_table.length then
    Static_table.table.(index - 1)
  else if index <= Static_table.length + table.Dynamic_table.length then
    Dynamic_table.get table (index - Static_table.length - 1)
  else
    raise Invalid_index

let header_field table max_size prefix prefix_length =
  let* index = any_int prefix prefix_length in
  let* name =
    if index = 0 then any_string max_size
    else match get_indexed_field table index with
      | name, _ -> return name
      | exception Invalid_index -> fail "invalid index" in
  let+ value = any_string max_size in
  (name, value)

let rec header ({table; max_size_limit; max_field_size} as decoder) =
  let* b = any_uint8 in
  if b >= 32 && b < 64 then
    let* max_size = any_int b 5 in
    if max_size <= max_size_limit then begin
      Dynamic_table.change_max_size table max_size;
      header decoder
    end else fail "exceeded size limit"
  else if table.max_size > max_size_limit then
    fail "exceeded size limit"
  else if b >= 128 then
    let* index = any_int b 7 in
    match get_indexed_field table index with
    | name, value -> return (Header.make name value)
    | exception Invalid_index -> fail "invalid index"
  else if b >= 64 then
    let* (name, value) = header_field table max_field_size b 6 in
    Dynamic_table.add table (name, value) |> ignore;
    return (Header.make name value)
  else
    let* (name, value) = header_field table max_field_size b 4 in
    return (Header.make ~never_index:(b >= 16) name value)

let headers t = many (header t) <* end_of_input

let change_table_size_limit ({table; _} as decoder) max_size_limit =
  if max_size_limit < 0 then
    raise (Invalid_argument "Decoder.change_table_size_limit");
  decoder.max_size_limit <- max_size_limit;
  if max_size_limit < table.max_size then
    Dynamic_table.change_max_size table max_size_limit
