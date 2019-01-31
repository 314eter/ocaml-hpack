exception Compression_error
exception Decoding_error

type header = {
  name : string;
  value : string;
  never_index : bool;
}
