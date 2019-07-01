open Crowbar
open Hpack

type entry = Headers of Header.t list | SizeUpdate of int * int


(* Generators *)

let pp_header ff Header.{name; value; never_index} =
  Format.fprintf ff "@[<hv 2>{ name = %S;@ value = %S;@ never_index = %s; }@]"
    name value (if never_index then "true" else "false")

let header =
  with_printer pp_header @@
  map [bytes; bytes; bool] @@ fun name value never_index ->
  Header.make ~never_index name value

let pp_entry ff = function
  | Headers headers ->
    Format.fprintf ff "Header %a" (pp_list pp_header) headers
  | SizeUpdate (size_limit, size) ->
    Format.fprintf ff "SizeUpdate (%d, %d)" size_limit size

let entry =
  with_printer pp_entry @@ choose [
    map [list header] (fun headers -> Headers headers);
    map [range 1000; range 9] begin fun size_limit d ->
      SizeUpdate (size_limit, size_limit / (d + 1))
    end;
  ]


(* Utilities *)

let decode decoder s =
  Angstrom.parse_string (Decoder.headers decoder) s

let encode encoder headers =
  let t = Faraday.create 1000 in
  List.iter (Encoder.encode_header encoder t) headers;
  Faraday.serialize_to_string t

let check_roundtrip encoder decoder headers =
  match encode encoder headers |> decode decoder with
  | Ok headers' -> check_eq ~pp:(pp_list pp_header) headers headers'
  | Error error -> fail error


(* Tests *)

let test_decode max_size_limit s =
  let decoder = Decoder.create ~max_size_limit () in
  match decode decoder s with
  | Ok _ -> ()
  | Error _ -> bad_test ()

let test_roundtrip max_size headers =
  let encoder = Encoder.create ~max_size () in
  let decoder = Decoder.create ~max_size_limit:max_size () in
  check_roundtrip encoder decoder headers

let test_complete entries =
  let encoder = Encoder.create () in
  let decoder = Decoder.create () in
  entries |> List.iter begin function
  | Headers headers -> check_roundtrip encoder decoder headers
  | SizeUpdate (size_limit, size) ->
    Decoder.change_table_size_limit decoder size_limit;
    Encoder.change_table_size encoder size
  end


let () =
  add_test ~name:"decode" [range 1000; bytes] test_decode;
  add_test ~name:"roundtrip" [range 1000; list header] test_roundtrip;
  add_test ~name:"complete" [list entry] test_complete
