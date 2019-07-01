type t = {
  name : string;
  value : string;
  never_index : bool;
}

let make ?(never_index=false) name value = {name; value; never_index}

let name {name; _} = name

let value {value; _} = value

let never_index {never_index; _} = never_index

let size {name; value; _} =
  String.length name + String.length value + 32
