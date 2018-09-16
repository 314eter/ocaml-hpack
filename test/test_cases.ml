open Hpack
open Hpack_lwt

module IO = struct
  type 'a t = 'a Lwt.t
  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel

  let read_byte ic =
    match%lwt Lwt_io.read_char ic with
    | c -> Lwt.return_some (int_of_char c)
    | exception End_of_file -> Lwt.return_none

  let write_byte oc b =
    Lwt_io.write_char oc (char_of_int b)

  let read_string ic length =
    let buffer = Bytes.create length in
    match%lwt Lwt_io.read_into_exactly ic buffer 0 length with
    | () -> Lwt.return_some (Bytes.unsafe_to_string buffer)
    | exception End_of_file -> Lwt.return_none

  let write_string = Lwt_io.write
end

module Encoder = Make_encoder (IO)
module Decoder = Make_decoder (IO)

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
  let encoder = Hpack.Encoder.create 4096 in
  Lwt_list.mapi_s begin fun seq (_, _, headers) ->
    let (ic, oc) = Lwt_io.pipe () in
    Lwt_list.iter_s (Encoder.encode_header encoder oc) headers ;%lwt
    Lwt_io.close oc ;%lwt
    let%lwt s = Lwt_io.read ic in
    Lwt_io.close ic ;%lwt
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

let header_equal {name; value; _} {name = name'; value = value'; _} =
  name = name' && value = value'

let decode cases =
  let decoder = Hpack.Decoder.create 65536 in
  Lwt_list.iter_s begin fun (size, s, headers) ->
    Hpack.Decoder.set_capacity decoder size;
    let (ic, oc) = Lwt_io.pipe () in
    Lwt_io.write oc s ;%lwt
    Lwt_io.close oc ;%lwt
    let headers' = ref headers in
    let%lwt error =
      Decoder.decode_headers decoder ic begin fun header ->
        if not (header_equal header (List.hd !headers')) then
          Lwt.fail_with (Hex.of_string s |> Hex.show)
        else Lwt.return (headers' := List.tl !headers')
      end in
    Lwt_io.close ic ;%lwt
    match error with
    | Ok () -> Lwt.return_unit
    | Error e -> Lwt.fail e
  end cases

let decode_file dir file =
  let cases = parse_file (dir ^ "/" ^ file) in
  decode cases

let encode_files () =
  Lwt_io.eprintlf "Encode ocaml-hpack" ;%lwt
  let files = Array.to_list (Sys.readdir "raw-data") in
  Lwt_list.iter_s encode_file files

let decode_files () =
  Lwt_list.iter_s begin fun dir ->
    Lwt_io.eprintlf "Decode %s" dir ;%lwt
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
  begin try Unix.mkdir "ocaml-hpack" 0o755
  with Unix.Unix_error (Unix.EEXIST, _, _) -> () end;
  Lwt_main.run begin
    encode_files () ;%lwt
    decode_files ()
  end
