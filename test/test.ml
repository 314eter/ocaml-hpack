open Hpack

type entry = Header of Header.t | Size of int

let header = (module struct
  type t = Header.t

  let pp fmt Header.{name; value; never_index} =
    Format.fprintf fmt "@[<hv 2>{ name = %S;@ value = %S;@ never_index = %s; }@]"
      name value (if never_index then "true" else "false")

  let equal = ( = )
end : Alcotest.TESTABLE with type t = Header.t)

let encoding = (module struct
  type t = string

  let pp fmt = Format.fprintf fmt "%S"

  let equal = ( = )
end : Alcotest.TESTABLE with type t = string)

let rec headers_of_entries = function
  | Header header :: entries -> header :: headers_of_entries entries
  | Size _ :: entries -> headers_of_entries entries
  | [] -> []

let test entries s = [
  "Encode", `Quick, begin fun () ->
    let t = Faraday.create 100 in
    let encoder = Encoder.create () in
    entries |> List.iter begin function
    | Header header -> Encoder.encode_header encoder t header
    | Size size -> Encoder.change_table_size encoder size
    end;
    let s' = Faraday.serialize_to_string t in
    Alcotest.(check encoding) "same encoding" s s'
  end;
  "Decode", `Quick, begin fun () ->
    let decoder = Decoder.create () in
    Alcotest.(check (result (list header) reject))
      "same headers"
      (Angstrom.parse_string (Decoder.headers decoder) s)
      (Ok (headers_of_entries entries))
  end;
]

let test_static =
  let entries = [
    Header (Header.make ":method" "PUT");
    Header (Header.make ":path" "/");
    Header (Header.make ":method" "PUT");
  ] in
  let s =
    "\x42\x03PUT\
     \x84\
     \xbe" in
  test entries s

let test_dynamic =
  let entries = [
    Header (Header.make "ABC" "ABC");
    Header (Header.make "ABC" "XYZ");
    Header (Header.make "ABC" "ABC");
  ] in
  let s =
    "\x40\x03ABC\x03ABC\
     \x7e\x03XYZ\
     \xbf" in
  test entries s

let test_huffman =
  let entries = [
    Header (Header.make "abc" "012");
  ] in
  let s = "\x40\x82\x1c\x64\x82\x00\x45" in
  test entries s

let test_no_index =
  let entries = [
    Header (Header.make "content-length" "350");
    Header (Header.make "content-length" "350");
  ] in
  let s =
    "\x0f\x0d\x03350\
     \x0f\x0d\x03350" in
  test entries s

let test_never_index =
  let entries = [
    Header (Header.make ~never_index:true "ABC" "XYZ");
    Header (Header.make ~never_index:true "ABC" "XYZ");
  ] in
  let s =
    "\x10\x03ABC\x03XYZ\
     \x10\x03ABC\x03XYZ" in
  test entries s

let test_size_update =
  let entries = [
    Header (Header.make "ABC" "XYZ");
    Size 0; Size 100;
    Header (Header.make "ABC" "XYZ");
  ] in
  let s =
    "\x40\x03ABC\x03XYZ\
     \x20\x3f\x45\
     \x40\x03ABC\x03XYZ" in
  test entries s

let test_eviction =
  let entries = [
    Size 38;
    Header (Header.make "ABC" "XYZ");
    Header (Header.make "ABCD" "XYZ");
    Header (Header.make "ABC" "XYZ");
    Header (Header.make "ABC" "XYZ");
    Header (Header.make "ABCD" "XYZ");
  ] in
  let s =
    "\x3f\x07\
     \x40\x03ABC\x03XYZ\
     \x40\x04ABCD\x03XYZ\
     \x40\x03ABC\x03XYZ\
     \xbe\
     \x40\x04ABCD\x03XYZ" in
  test entries s

let () =
  Alcotest.run "Hpack" [
    "Static Indexing", test_static; (* RFC7541§2.3.1 *)
    "Dynamic Indexing", test_dynamic; (* RFC7541§2.3.2 *)
    "Huffman Encoding", test_huffman; (* RFC7541§5.2 *)
    "No Indexing", test_no_index; (* RFC7541§6.2.2 *)
    "Never Index", test_never_index; (* RFC7541§7.1.3 *)
    "Table Size Update", test_size_update; (* RFC7541§4.2 *)
    "Entry Eviction", test_eviction; (* RFC7541§4.4 *)
  ]
