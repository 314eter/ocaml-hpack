open Hpack

type entry = Header of header | Size of int

let header = (module struct
  type t = header

  let pp fmt {name; value; never_index} =
    Format.fprintf fmt "@[<hv 2>{ name = %S;@ value = %S;@ never_index = %s; }@]"
      name value (if never_index then "true" else "false")

  let equal = ( = )
end : Alcotest.TESTABLE with type t = header)

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

let test_eviction =
  let entries = [
    Size 38;
    Header {name = "ABC"; value = "XYZ"; never_index = false};
    Header {name = "ABCD"; value = "XYZ"; never_index = false};
    Header {name = "ABC"; value = "XYZ"; never_index = false};
    Header {name = "ABC"; value = "XYZ"; never_index = false};
    Header {name = "ABCD"; value = "XYZ"; never_index = false};
  ] in
  let s =
    "\x3f\x07\
     \x40\x03ABC\x03XYZ\
     \x40\x04ABCD\x03XYZ\
     \x40\x03ABC\x03XYZ\
     \xbe\
     \x40\x04ABCD\x03XYZ" in
  test entries s

let test_size_update =
  let entries = [
    Header {name = "ABC"; value = "XYZ"; never_index = false};
    Header {name = "ABC"; value = "XYZ"; never_index = false};
    Size 0; Size 100;
    Header {name = "ABC"; value = "XYZ"; never_index = false};
  ] in
  let s =
    "\x40\x03ABC\x03XYZ\
     \xbe\
     \x20\x3f\x45\
     \x40\x03ABC\x03XYZ" in
  test entries s

let test_never_index =
  let entries = [
    Header {name = "ABC"; value = "XYZ"; never_index = true};
    Header {name = "ABC"; value = "XYZ"; never_index = true};
  ] in
  let s =
    "\x10\x03ABC\x03XYZ\
     \x10\x03ABC\x03XYZ" in
  test entries s

let () =
  Alcotest.run "Hpack" [
    "Entry Eviction", test_eviction;
    "Table Size Update", test_size_update;
    "Never Index", test_never_index;
  ]
