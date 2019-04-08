type t = {
  mutable entries : (string * string) array;
  (* A circular buffer containing name value pairs *)
  mutable capacity : int;
  (* The size of the circular buffer, equal to [Array.length entries]
     The capacity is increased when the buffer is full. *)
  mutable offset : int;
  (* The offset in the circular buffer
     This changes when entries are evicted. *)
  mutable length : int;
  (* The number of entries in the table *)
  mutable size : int;
  (* The size of the dynamic table according to the specification *)
  mutable max_size : int;
  (* The maximum allowed size of the dynamic table
     If this limit is reached, entries are evicted. *)
  evict_callback : (string * string) -> unit;
  (* [evict_callback entry] is called when [entry] is evicted *)
}

let default_entry = ("", "")
let default_evict_callback = ignore

let create ?(evict_callback=default_evict_callback) max_size = {
  entries = Array.make 128 default_entry;
  capacity = 128;
  offset = 0;
  length = 0;
  size = 0;
  max_size;
  evict_callback;
}

let get {entries; capacity; offset; _} i =
  entries.((offset + i) mod capacity)

let entry_size (name, value) =
  String.length name + String.length value + 32

let evict_one ({entries; capacity; offset; length; evict_callback; _} as table) =
  let i = (offset + length - 1) mod capacity in
  let entry = entries.(i) in
  entries.(i) <- default_entry;
  table.length <- length - 1;
  table.size <- table.size - entry_size entry;
  evict_callback entry

let increase_capacity table =
  let capacity = 2 * table.capacity in
  let entries = Array.init capacity begin fun i ->
      if i < table.length then get table i
      else default_entry
    end in
  table.entries <- entries;
  table.capacity <- capacity;
  table.offset <- 0

let add table entry =
  let entry_size = entry_size entry in
  while table.size > 0 && table.size + entry_size > table.max_size do
    evict_one table
  done;
  if table.size + entry_size <= table.max_size then begin
    if table.length = table.capacity then
      increase_capacity table;
    let offset = (table.offset + table.capacity - 1) mod table.capacity in
    table.entries.(offset) <- entry;
    table.offset <- offset;
    table.length <- table.length + 1;
    table.size <- table.size + entry_size;
    true
  end else false

let change_max_size table max_size =
  table.max_size <- max_size;
  while table.size > table.max_size do
    evict_one table
  done
