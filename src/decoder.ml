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

module Make (C : C) (IO : IO with type 'a t = 'a C.t) = struct
  module Let_syntax = struct
    let bind x ~f = C.bind x f
    let map x ~f = C.map f x
  end

  module R = struct
    let return x = C.return (Ok x)
    let fail e = C.return (Error e)

    module Let_syntax = struct
      let bind x ~f =
        match%bind x with
        | Ok x -> f x
        | Error e -> fail e

      let map x ~f =
        match%map x with
        | Ok x -> Ok (f x)
        | Error e -> Error e
    end
  end

  let read_byte ic =
    match%bind IO.read_byte ic with
    | Some b -> R.return b
    | None -> R.fail End_of_file

  let read_string ic length =
    match%bind IO.read_string ic length with
    | Some s -> R.return s
    | None -> R.fail End_of_file

  let decode_int ic prefix prefix_length =
    let max_prefix = 1 lsl prefix_length - 1 in
    let i = prefix land max_prefix in
    if i < max_prefix then
      R.return i
    else
      let rec loop i k =
        let%bind.R b = read_byte ic in
        let i = i + (b land 127) lsl k in
        if b >= 128 then
          loop i (k + 7)
        else R.return i in
      loop i 0

  let decode_string ic =
    let%bind.R b = read_byte ic in
    let%bind.R length = decode_int ic b 7 in
    let%bind.R s = read_string ic length in
    if b < 128 then R.return s
    else match Huffman.decode s with
      | s -> R.return s
      | exception Compression_error -> R.fail Compression_error

  let get_indexed_field table index =
    if index = 0 then
      raise Decoding_error
    else if index <= Static_table.size then
      Static_table.table.(index - 1)
    else
      Dynamic_table.get table (index - Static_table.size - 1)

  let _decode_header_field table ic prefix prefix_length =
    let%bind.R index = decode_int ic prefix prefix_length in
    let%bind.R name =
      if index = 0 then decode_string ic
      else match get_indexed_field table index with
        | name, _ -> R.return name
        | exception Decoding_error -> R.fail Decoding_error in
    let%map.R value = decode_string ic in
    (name, value)

  let rec decode_headers ({table; _} as decoder) ic f =
    match%bind read_byte ic with
    | Error End_of_file -> R.return ()
    | Error e -> R.fail e
    | Ok b ->
      if b >= 128 then
        let%bind.R index = decode_int ic b 7 in
        match get_indexed_field table index with
        | name, value ->
          let%bind () = f { name; value; never_index = false } in
          decode_headers decoder ic f
        | exception Decoding_error -> R.fail Decoding_error
      else if b >= 64 then
        let%bind.R (name, value) = _decode_header_field table ic b 6 in
        Dynamic_table.add table (name, value);
        let%bind () = f { name; value; never_index = false } in
        decode_headers decoder ic f
      else if b < 32 then
        let%bind.R (name, value) = _decode_header_field table ic b 4 in
        let%bind () = f { name; value; never_index = false } in
        decode_headers decoder ic f
      else
        let%bind.R capacity = decode_int ic b 5 in
        match set_capacity decoder capacity with
        | () -> decode_headers decoder ic f
        | exception Decoding_error -> R.fail Decoding_error
end
