open Crowbar
open Hpack

let pp_header ff {name; value; never_index} =
  Format.fprintf ff "@[<hv 2>{ name = %S;@ value = %S;@ never_index = %s; }@]"
    name value (if never_index then "true" else "false")

let header =
  with_printer pp_header @@
  map [bytes; bytes; bool] @@ fun name value never_index ->
  {name; value; never_index}

let decode decoder s =
  Angstrom.parse_string (Decoder.headers decoder) s

let encode encoder headers =
  let t = Faraday.create 1000 in
  List.iter (Encoder.encode_header encoder t) headers;
  Faraday.serialize_to_string t

let test_decode capacity s =
  let decoder = Decoder.create capacity in
  decode decoder s |> ignore

let test_roundtrip capacity headers =
  let encoder = Encoder.create capacity in
  let decoder = Decoder.create capacity in
  let s = encode encoder headers in
  match decode decoder s with
  | Ok headers' -> check_eq ~pp:(pp_list pp_header) headers headers'
  | Error error -> fail error

let () =
  add_test ~name:"decode" [range 1000; bytes] test_decode;
  add_test ~name:"roundtrip" [range 1000; list header] test_roundtrip;
