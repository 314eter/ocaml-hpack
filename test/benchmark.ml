open Hpack

module Json = Yojson.Basic.Util

module Hex = struct
  let of_string s = Hex.show (Hex.of_string s)
  let to_string x = Hex.to_string (`Hex x)
end

let parse_file file =
  Yojson.Basic.from_file file
  |> Json.member "cases"
  |> Json.convert_each begin fun case ->
    Json.member "headers" case
    |> Json.to_list
    |> Json.filter_assoc
    |> List.flatten
    |> List.map begin fun (name, value) ->
      Header.make name (Json.to_string value)
    end
  end
  |> List.flatten

let parse_dir dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort String.compare
  |> List.map (( ^ ) (dir ^ "/"))
  |> List.map parse_file
  |> List.flatten

let time () =
  let Unix.{tms_utime; _} = Unix.times () in tms_utime

let encode headers =
  let t = Faraday.create 360000 in
  let encoder = Encoder.create () in
  List.iter (Encoder.encode_header encoder t) headers;
  Faraday.serialize_to_string t

let decode s =
  let decoder = Decoder.create () in
  Angstrom.parse_string (Decoder.headers decoder) s

let () =
  let headers = parse_dir Sys.argv.(1) in
  let start = time () in
  for _ = 1 to 199 do encode headers |> ignore done;
  let s = encode headers in
  Printf.printf "Encoding time: %f\n" (time () -. start);
  let start = time () in
  for _ = 1 to 200 do decode s |> ignore done;
  Printf.printf "Decoding time: %f\n" (time () -. start)
