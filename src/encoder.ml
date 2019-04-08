open S

module F = Faraday

module TokenSet = Set.Make (struct
    type t = int
    let compare : int -> int -> int = compare
  end)

module LookupTable = Hashtbl.Make (struct
    type t = string
    let equal = String.equal
    let hash : string -> int = Hashtbl.hash
  end)

module ValueMap = Map.Make (String)

type t = {
  table : Dynamic_table.t;
  mutable min_table_size_change : int option;
  lookup_table : int ValueMap.t LookupTable.t;
  mutable next_seq : int;
}

type encoding =
  | Never_index of int
  | No_index of int
  | Do_index of int
  | Index of int

let on_evict lookup_table (name, value) =
  let map = LookupTable.find lookup_table name in
  if ValueMap.cardinal map = 1 then
    LookupTable.remove lookup_table name
  else
    let map = ValueMap.remove value map in
    LookupTable.replace lookup_table name map

let create ?(max_size=4096) () =
  let lookup_table = LookupTable.create 128 in
  let table = Dynamic_table.create ~on_evict:(on_evict lookup_table) max_size in
  let min_table_size_change = if max_size != 4096 then Some max_size else None in
  {table; min_table_size_change; lookup_table; next_seq = 0}

let add ({table; lookup_table; next_seq; _} as encoder) ((name, value) as entry) =
  if Dynamic_table.add table entry then
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
  Static_table.length + next_seq - seq

let is_never_index token value =
  match token with
  | Some token ->
    token == Static_table.Token.authorization ||
    (token == Static_table.Token.cookie && String.length value < 20)
  | None -> false

let no_index_tokens =
  TokenSet.of_list Static_table.Token.[
      _path;
      age;
      content_length;
      etag;
      if_modified_since;
      if_none_match;
      location;
      set_cookie;
    ]

let is_no_index token =
  match token with
  | Some token -> TokenSet.mem token no_index_tokens
  | None -> false

let encode ({lookup_table; next_seq; _} as encoder) {name; value; never_index} =
  let token = Static_table.lookup_token name in
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

let encode_int t prefix prefix_length i =
  let max_prefix = 1 lsl prefix_length - 1 in
  if i < max_prefix then
    F.write_uint8 t (prefix lor i)
  else
    let i = i - max_prefix in
    F.write_uint8 t (prefix lor max_prefix);
    let rec loop i =
      if i >= 128 then begin
        F.write_uint8 t ((i land 127) lor 128);
        loop (i lsr 7)
      end else F.write_uint8 t i in
    loop i

let encode_string t s =
  let length = String.length s in
  let encoded_length = Huffman.encoded_length s in
  if encoded_length < length then begin
    encode_int t 128 7 encoded_length;
    Huffman.encode t s
  end else begin
    encode_int t 0 7 length;
    F.write_string t s
  end

let _encode_header t prefix prefix_length index name value =
  encode_int t prefix prefix_length index;
  if index = 0 then begin
    encode_string t name;
    encode_string t value
  end else encode_string t value

let encode_header ({table; _} as encoder) t ({name; value; _} as header) =
  begin match encoder.min_table_size_change with
  | Some min_max_size ->
    encoder.min_table_size_change <- None;
    encode_int t 32 5 min_max_size;
    if table.size > min_max_size then
      encode_int t 32 5 table.size;
  | None -> ()
  end;
  begin match encode encoder header with
  | Never_index index -> _encode_header t 16 4 index name value
  | No_index index -> _encode_header t 0 4 index name value
  | Do_index index -> _encode_header t 64 6 index name value
  | Index index -> encode_int t 128 7 index
  end

let change_table_size encoder max_size =
  Dynamic_table.change_max_size encoder.table max_size;
  match encoder.min_table_size_change with
  | Some min_max_size when max_size < min_max_size ->
    encoder.min_table_size_change <- Some max_size
  | _ -> ()
