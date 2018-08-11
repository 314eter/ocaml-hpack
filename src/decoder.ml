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

module Make (IO : IO) = struct
  module Let_syntax = struct
    let bind x ~f = IO.bind x f
    let map x ~f = IO.map f x
  end

  let decode_int ic prefix prefix_length =
    let max_prefix = 1 lsl prefix_length - 1 in
    let i = prefix land max_prefix in
    if i < max_prefix then
      IO.return (i, 0)
    else
      let rec loop i k =
        let%bind b = IO.read_byte ic in
        let i = i + (b land 127) lsl (7 * k) in
        if b >= 128 then
          loop i (k + 1)
        else IO.return (i, k + 1) in
      loop i 0

  let decode_string ic =
    let%bind b = IO.read_byte ic in
    let%bind (length, k) = decode_int ic b 7 in
    let%map s = IO.read_string ic length in
    if b >= 128 then
      (Huffman.decode s, 1 + k + length)
    else
      (s, 1 + k + length)

  let get_indexed_field table index =
    if index = 0 then
      raise Decoding_error
    else if index <= Static_table.size then
      Static_table.table.(index - 1)
    else
      Dynamic_table.get table (index - Static_table.size - 1)

  let _decode_header_field table ic prefix prefix_length =
    let%bind (index, k) = decode_int ic prefix prefix_length in
    let%bind (name, k') =
      if index = 0 then decode_string ic
      else IO.return (fst (get_indexed_field table index), 0) in
    let%map (value, k'') = decode_string ic in
    (name, value, k + k' + k'')

  let rec decode_headers ({table; _} as decoder) ic = function
    | 0 -> IO.return []
    | length ->
      let%bind b = IO.read_byte ic in
      if b >= 128 then
        let%bind (index, k) = decode_int ic b 7 in
        let (name, value) = get_indexed_field table index in
        let%map headers = decode_headers decoder ic (length - 1 - k) in
        { name; value; never_index = false } :: headers
      else if b >= 64 then
        let%bind (name, value, k) = _decode_header_field table ic b 6 in
        Dynamic_table.add table (name, value);
        let%map headers = decode_headers decoder ic (length - 1 - k) in
        { name; value; never_index = false } :: headers
      else if b < 32 then
        let%bind (name, value, k) = _decode_header_field table ic b 4 in
        let%map headers = decode_headers decoder ic (length - 1 - k) in
        { name; value; never_index = (b >= 16) } :: headers
      else
        let%bind (capacity, k) = decode_int ic b 5 in
        set_capacity decoder capacity;
        decode_headers decoder ic (length - 1 - k)
end
