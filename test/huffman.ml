let example = "www.example.com"
let example_huffman = "\xf1\xe3\xc2\xe5\xf2\x3a\x6b\xa0\xab\x90\xf4\xff"

let encode_example () =
  Alcotest.(check string) "equal" (Hyper.Huffman.encode example) example_huffman

let decode_example () =
  Alcotest.(check string) "equal" (Hyper.Huffman.decode example_huffman) example

let test_example = [
  "encode", `Quick, encode_example;
  "decode", `Quick, decode_example;
]

let () =
  Alcotest.run "Example" [
    "www.example.com", test_example
  ]
