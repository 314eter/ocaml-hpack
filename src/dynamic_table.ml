type t = {
  mutable entries : (string * string) array;
  mutable size : int;
  mutable offset : int;
  mutable capacity : int;
  mutable hpack_size : int;
  mutable hpack_capacity : int;
  evict_callback : (string * string) -> unit;
}

let default_entry = ("", "")
let default_evict_callback = ignore

let create ?(evict_callback=default_evict_callback) hpack_capacity =
  let capacity = max 16 hpack_capacity in
  {
    entries = Array.make capacity default_entry;
    size = 0;
    offset = 0;
    capacity;
    hpack_size = 0;
    hpack_capacity;
    evict_callback;
  }

let get table i =
  table.entries.((table.offset + i) mod table.capacity)

let hpack_size_entry (name, value) =
  String.length name + String.length value + 32

let evict_one table =
  table.size <- table.size - 1;
  let i = (table.offset + table.size) mod table.capacity in
  let entry = table.entries.(i) in
  table.entries.(i) <- default_entry;
  table.hpack_size <- table.hpack_size - hpack_size_entry entry;
  table.evict_callback entry

let increase_capacity table =
  let new_capacity = 2 * table.capacity in
  let new_entries = Array.init new_capacity begin fun i ->
      if i < table.size then get table i
      else default_entry
    end in
  table.entries <- new_entries;
  table.offset <- 0;
  table.capacity <- new_capacity

let add table entry =
  let entry_size = hpack_size_entry entry in
  while table.hpack_size > 0 && table.hpack_size + entry_size > table.hpack_capacity do
    evict_one table
  done;
  if table.hpack_capacity >= table.hpack_size + entry_size then begin
    if table.size = table.capacity then increase_capacity table;
    table.size <- table.size + 1;
    table.hpack_size <- table.hpack_size + entry_size;
    table.offset <- (table.offset + table.capacity - 1) mod table.capacity;
    table.entries.(table.offset) <- entry;
  end

let set_capacity table hpack_capacity =
  table.hpack_capacity <- hpack_capacity;
  while table.hpack_size > table.hpack_capacity do
    evict_one table
  done
