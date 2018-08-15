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
  let decode_int ic prefix prefix_length =
    let max_prefix = 1 lsl prefix_length - 1 in
    let i = prefix land max_prefix in
    if i < max_prefix then
      Lwt.return i
    else
      let rec loop i k =
        let%lwt b = IO.read_byte ic in
        let i = i + (b land 127) lsl k in
        if b >= 128 then
          loop i (k + 7)
        else Lwt.return i in
      loop i 0

  let decode_string ic =
    let%lwt b = IO.read_byte ic in
    let%lwt length = decode_int ic b 7 in
    let%lwt s = IO.read_string ic length in
    Lwt.return (if b >= 128 then Huffman.decode s else s)

  let get_indexed_field table index =
    if index = 0 then
      raise Decoding_error
    else if index <= Static_table.size then
      Static_table.table.(index - 1)
    else
      Dynamic_table.get table (index - Static_table.size - 1)

  let _decode_header_field table ic prefix prefix_length =
    let%lwt index = decode_int ic prefix prefix_length in
    let%lwt name =
      if index = 0 then decode_string ic
      else Lwt.return (fst (get_indexed_field table index)) in
    let%lwt value = decode_string ic in
    Lwt.return (name, value)

  let rec decode_headers ({table; _} as decoder) ic =
    match%lwt IO.read_byte ic with
    | b ->
      if b >= 128 then
        let%lwt index = decode_int ic b 7 in
        let (name, value) = get_indexed_field table index in
        let%lwt headers = decode_headers decoder ic in
        Lwt.return @@ { name; value; never_index = false } :: headers
      else if b >= 64 then
        let%lwt (name, value) = _decode_header_field table ic b 6 in
        Dynamic_table.add table (name, value);
        let%lwt headers = decode_headers decoder ic in
        Lwt.return @@ { name; value; never_index = false } :: headers
      else if b < 32 then
        let%lwt (name, value) = _decode_header_field table ic b 4 in
        let%lwt headers = decode_headers decoder ic in
        Lwt.return @@ { name; value; never_index = (b >= 16) } :: headers
      else
        let%lwt capacity = decode_int ic b 5 in
        set_capacity decoder capacity;
        decode_headers decoder ic
    | exception End_of_file -> Lwt.return_nil
end
