open Hpack

module Json = Yojson.Basic.Util

module Hex = struct
  let of_string s = Hex.show (Hex.of_string s)
  let to_string x = Hex.to_string (`Hex x)
end

type case = {
  header_table_size : int option;
  wire : string;
  headers : header list;
}

let parse_file file =
  Yojson.Basic.from_file file
  |> Json.member "cases"
  |> Json.convert_each begin fun case -> {
      header_table_size = Json.member "header_table_size" case |> Json.to_int_option;
      wire =
        begin match Json.member "wire" case |> Json.to_string_option with
        | Some s -> Hex.to_string s
        | None -> ""
        end;
      headers =
        Json.member "headers" case
        |> Json.to_list
        |> Json.filter_assoc
        |> List.flatten
        |> List.map begin fun (name, value) ->
          {name; value = Json.to_string value; never_index = false}
        end
    }
  end

let encode cases =
  let encoder = Encoder.create 4096 in
  cases |> List.map begin fun {headers; _} ->
    let t = Faraday.create 1200 in
    List.iter (Encoder.encode_header encoder t) headers;
    let wire = Faraday.serialize_to_string t in
    {header_table_size = None; wire; headers}
  end

let encode_file file =
  let cases = parse_file ("raw-data/" ^ file) in
  let result = encode cases in
  let json =
    Yojson.pretty_to_string @@ `Assoc [
      "description", `String "Encoded by ocaml-hpack";
      "cases", `List (
        result |> List.mapi @@ fun seqno {wire; headers; _} ->
        `Assoc [
          "seqno", `Int seqno;
          "wire", `String (Hex.of_string wire);
          "headers", `List (
            headers |> List.map @@ fun {name; value; _} ->
            `Assoc [(name, `String value)]
          )
        ]
      )
    ] in
  let oc = open_out ("ocaml-hpack/" ^ file) in
  output_string oc json;
  close_out oc

let encode_files () =
  prerr_endline "Encode ocaml-hpack";
  let files = Array.to_list (Sys.readdir "raw-data") in
  List.iter encode_file files

let header_equal ({name; value; _}, {name = name'; value = value'; _}) =
  name = name' && value = value'

let decode cases =
  let decoder = Decoder.create 65536 in
  List.iter begin fun {header_table_size; wire; headers; _} ->
    begin match header_table_size with
    | Some capacity -> Decoder.set_capacity decoder capacity
    | None -> ()
    end;
    match Angstrom.parse_string (Decoder.headers decoder) wire with
    | Ok headers' when (List.combine headers headers' |> List.for_all header_equal) -> ()
    | Ok _ -> failwith (Hex.of_string wire)
    | Error error -> failwith error
  end cases

let decode_file dir file =
  let cases = parse_file (dir ^ "/" ^ file) in
  decode cases

let decode_files () =
  List.iter begin fun dir ->
    Printf.eprintf "Decode %s\n" dir;
    Sys.readdir dir
    |> Array.to_list
    |> List.iter (decode_file dir)
  end [
    "ocaml-hpack";
    "go-hpack";
    "haskell-http2-linear";
    "haskell-http2-linear-huffman";
    "haskell-http2-naive";
    "haskell-http2-naive-huffman";
    "haskell-http2-static";
    "haskell-http2-static-huffman";
    "nghttp2";
    "nghttp2-16384-4096";
    "nghttp2-change-table-size";
    "node-http2-hpack";
    "python-hpack";
  ]

let () =
  Sys.chdir Sys.argv.(1);
  begin try Unix.mkdir "ocaml-hpack" 0o755
  with Unix.Unix_error (Unix.EEXIST, _, _) -> () end;
  encode_files ();
  decode_files ()
