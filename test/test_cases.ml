open Hpack

let parse_file file =
  match Yojson.Safe.from_file file with
  | `Assoc l ->
    begin match List.assoc "cases" l with
    | `List cases ->
      List.map begin function
      | `Assoc l ->
        begin match List.assoc_opt "header_table_size" l with
        | Some (`Int size) -> size
        | _ -> 4096
        end,
        begin match List.assoc_opt "wire" l with
        | Some (`String hex) -> Hex.to_string (`Hex hex)
        | _ -> ""
        end,
        begin match List.assoc "headers" l with
        | `List headers ->
          List.map begin function
          | `Assoc [(name, `String value)] ->
            {name; value; never_index = false}
          | _ -> assert false
          end headers
        | _ -> assert false
        end
      | _ -> assert false
      end cases
    | _ -> assert false
    end
  | _ -> assert false

let encode cases =
  let encoder = Encoder.create 4096 in
  List.mapi begin fun seq (_, _, headers) ->
    let t = Faraday.create 0x1000 in
    List.iter (Encoder.encode_header encoder t) headers;
    let s = Faraday.serialize_to_string t in
    (seq, Hex.of_string s |> Hex.show, headers)
  end cases

let encode_file file =
  let cases = parse_file ("raw-data/" ^ file) in
  let result = encode cases in
  let json =
    Yojson.pretty_to_string @@ `Assoc [
      "description", `String "Encoded by ocaml-hpack";
      "cases", `List (
        result |> List.map @@ fun (seq, wire, headers) ->
        `Assoc [
          "seqno", `Int seq;
          "wire", `String wire;
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

let header_equal ({name; value; _}, {name = name'; value = value'; _}) =
  name = name' && value = value'

let decode cases =
  let decoder = Decoder.create 65536 in
  List.iter begin fun (size, s, headers) ->
    Decoder.set_capacity decoder size;
    match Angstrom.parse_string (Decoder.headers decoder) s with
    | Ok headers' when (List.combine headers headers' |> List.for_all header_equal) -> ()
    | Ok _ -> failwith (Hex.of_string s |> Hex.show)
    | Error error -> failwith error
  end cases

let decode_file dir file =
  let cases = parse_file (dir ^ "/" ^ file) in
  decode cases

let encode_files () =
  prerr_endline "Encode ocaml-hpack";
  let files = Array.to_list (Sys.readdir "raw-data") in
  List.iter encode_file files

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
