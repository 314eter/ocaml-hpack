let rfc7541 =
  let b = Buffer.create 1000 in
  let ic = open_in "rfc7541.html" in
  begin try while true do Buffer.add_channel b ic 1000 done
  with End_of_file -> close_in ic end;
  Buffer.contents b

let () =
  let huffman = Hyper.Huffman.encode rfc7541 in
  print_string huffman;
  let plain = Hyper.Huffman.decode huffman in
  print_string plain
