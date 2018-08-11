open S

module TokenSet = Set.Make (struct
    type t = int
    let compare (x : int) (y : int) = compare x y
  end)

module LookupTable = Hashtbl.Make (struct
    type t = string
    let equal = String.equal
    let hash (s : string) = Hashtbl.hash s
  end)

module ValueMap = Map.Make (String)

type t = {
  table : Dynamic_table.t;
  lookup_table : int ValueMap.t LookupTable.t;
  mutable next_seq : int;
}

type encoding =
  | Never_index of int
  | No_index of int
  | Do_index of int
  | Index of int

let evicted lookup_table (name, value) =
  let map = LookupTable.find lookup_table name in
  if ValueMap.cardinal map = 1 then
    LookupTable.remove lookup_table name
  else
    let map = ValueMap.remove value map in
    LookupTable.replace lookup_table name map

let create capacity =
  let lookup_table = LookupTable.create capacity in
  {
    table = Dynamic_table.create ~evict_callback:(evicted lookup_table) capacity;
    lookup_table;
    next_seq = 0;
  }

let add ({table; lookup_table; next_seq} as encoder) ((name, value) as entry) =
  Dynamic_table.add table entry;
  let map =
    match LookupTable.find_opt lookup_table name with
    | Some map -> ValueMap.add value next_seq map
    | None -> ValueMap.singleton value next_seq in
  encoder.next_seq <- next_seq + 1;
  LookupTable.replace lookup_table name map

let find_token encoder no_index token name value =
  let rec loop i =
    let (name', value') = Static_table.table.(i) in
    if name' <> name then
      if no_index then No_index (token + 1) else begin
        add encoder (name, value);
        Do_index (token + 1)
      end
    else if value' = value then
      Index (i + 1)
    else loop (i + 1) in
  loop token

let seq_to_index next_seq seq =
  Static_table.size + next_seq - seq

let is_never_index token value =
  match token with
  | Some token ->
    token == Static_table.token_authorization ||
    (token == Static_table.token_cookie && String.length value < 20)
  | None -> false

let no_index_tokens =
  TokenSet.of_list Static_table.[
      token__path;
      token_age;
      token_content_length;
      token_etag;
      token_if_modified_since;
      token_if_none_match;
      token_location;
      token_set_cookie;
    ]

let is_no_index token =
  match token with
  | Some token -> TokenSet.mem token no_index_tokens
  | None -> false

let encode ({lookup_table; next_seq; _} as encoder) {name; value; never_index} =
  let token = Static_table.lookup_token name in
  let never_index = never_index || is_never_index token value in
  let no_index = is_no_index token in
  if never_index || is_never_index token value then
    match token with
    | Some token -> Never_index (token + 1)
    | None ->
      match LookupTable.find_opt lookup_table name with
      | Some map ->
        Never_index (seq_to_index next_seq (snd (ValueMap.choose map)))
      | None -> Never_index 0
  else
    match token, LookupTable.find_opt lookup_table name with
    | Some token, Some map ->
      begin match ValueMap.find_opt value map with
      | Some seq -> Index (seq_to_index next_seq seq)
      | None -> find_token encoder no_index token name value
      end
    | Some token, None -> find_token encoder no_index token name value
    | None, Some map ->
      begin match ValueMap.find_opt value map with
      | Some seq -> Index (seq_to_index next_seq seq)
      | None ->
        let index = seq_to_index next_seq (snd (ValueMap.choose map)) in
        if no_index then No_index index else begin
          add encoder (name, value);
          Do_index index
        end
      end
    | None, None ->
      if no_index then No_index 0 else begin
        add encoder (name, value);
        Do_index 0
      end

module Make (IO : IO) = struct
  module Let_syntax = struct
    let bind x ~f = IO.bind x f
  end

  let encode_int oc prefix prefix_length i =
    let max_prefix = 1 lsl prefix_length - 1 in
    if i < max_prefix then
      IO.write_byte oc (prefix lor i)
    else
      let i = i - max_prefix in
      let%bind () = IO.write_byte oc (prefix lor max_prefix) in
      let rec loop i =
        if i >= 128 then begin
          let%bind () = IO.write_byte oc ((i land 127) lor 128) in
          loop (i lsr 7)
        end else IO.write_byte oc i in
      loop i

  let encode_string oc s =
    let (prefix, s) =
      if Huffman.encoded_length s >= String.length s then (0, s)
      else (128, Huffman.encode s) in
    let%bind () = encode_int oc prefix 7 (String.length s) in
    IO.write_string oc s

  let encode_header oc prefix prefix_length index name value =
    let%bind () = encode_int oc prefix prefix_length index in
    if index = 0 then
      let%bind () = encode_string oc name in
      encode_string oc value
    else encode_string oc value

  let rec encode_headers encoder oc = function
    | [] -> IO.return ()
    | {name; value; _} as header :: headers ->
      let%bind () =
        match encode encoder header with
        | Never_index index -> encode_header oc 16 4 index name value
        | No_index index -> encode_header oc 0 4 index name value
        | Do_index index -> encode_header oc 64 6 index name value
        | Index index -> encode_int oc 128 7 index in
      encode_headers encoder oc headers
end
