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
  Lwt_list.mapi_s begin fun seq (_, _, headers) ->
    let (ic, oc) = Lwt_io.pipe () in
    Encoder_lwt.encode_headers encoder oc headers;%lwt
    Lwt_io.close oc;%lwt
    let%lwt s = Lwt_io.read ic in
    Lwt_io.close ic;%lwt
    Lwt.return (seq, Hex.of_string s |> Hex.show, headers)
  end cases

let encode_file file =
  let cases = parse_file ("raw-data/" ^ file) in
  let%lwt result = encode cases in
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
  Lwt_io.with_file ~mode:Lwt_io.output ("ocaml-hpack/" ^ file) @@ fun oc ->
  Lwt_io.write oc json

let rec headers_equal headers headers' =
  match headers, headers' with
  | {name; value; _} :: headers,
    {name = name'; value = value'; _} :: headers'
    when name = name' && value = value' ->
    headers_equal headers headers'
  | [], [] -> true
  | _ -> false

let decode cases =
  let (ic, oc) = Lwt_io.pipe () in
  let decoder = Decoder.create 65536 in
  Lwt_list.iter_s begin fun (size, s, headers) ->
    Decoder.set_capacity decoder size;
    Lwt_io.write oc s;%lwt
    let%lwt headers' = Decoder_lwt.decode_headers decoder ic (String.length s) in
    if not (headers_equal headers headers') then begin
      Lwt_list.iter_s begin fun {name; value; _} ->
        Lwt_io.eprintlf "%s\t%s" name value
        end headers';%lwt
      Lwt.fail_with (Hex.of_string s |> Hex.show)
    end else Lwt.return_unit
  end cases

let decode_file dir file =
  let cases = parse_file (dir ^ "/" ^ file) in
  decode cases

let encode_files () =
  Lwt_io.eprintlf "Encode ocaml-hpack";%lwt
  let files = Array.to_list (Sys.readdir "raw-data") in
  Lwt_list.iter_s encode_file files

let decode_files () =
  Lwt_list.iter_s begin fun dir ->
    Lwt_io.eprintlf "Decode %s" dir;%lwt
    Sys.readdir dir
    |> Array.to_list
    |> Lwt_list.iter_s (decode_file dir)
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
  begin try Unix.mkdir "ocaml-hpack" 0o644
  with Unix.Unix_error (Unix.EEXIST, _, _) -> () end;
  Lwt_main.run begin
    encode_files ();%lwt
    decode_files ()
  end
