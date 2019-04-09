type t = {
  mutable entries : (string * string) array;
  (* A circular buffer containing name value pairs *)
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
  on_evict : (string * string) -> unit;
  (* [on_evict entry] is called when [entry] is evicted *)
}

let default_entry = ("", "")

let create ?(on_evict=ignore) max_size =
  let entries = Array.make 128 default_entry in
  {entries; offset = 0; length = 0; size = 0; max_size; on_evict}

let get {entries; offset; _} i =
  entries.((offset + i) mod Array.length entries)

let entry_size (name, value) =
  String.length name + String.length value + 32

let evict_one ({entries; offset; length; on_evict; _} as table) =
  let i = (offset + length - 1) mod Array.length entries in
  let entry = entries.(i) in
  entries.(i) <- default_entry;
  table.length <- length - 1;
  table.size <- table.size - entry_size entry;
  on_evict entry

let resize table capacity =
  let entries = Array.init capacity begin fun i ->
      if i < table.length then get table i
      else default_entry
    end in
  table.entries <- entries;
  table.offset <- 0

let add table entry =
  let entry_size = entry_size entry in
  while table.size > 0 && table.size + entry_size > table.max_size do
    evict_one table
  done;
  if table.size + entry_size <= table.max_size then begin
    let capacity = Array.length table.entries in
    if table.length = capacity then
      resize table (2 * capacity);
    let capacity = Array.length table.entries in
    let offset = (table.offset + capacity - 1) mod capacity in
    table.entries.(offset) <- entry;
    table.offset <- offset;
    table.length <- table.length + 1;
    table.size <- table.size + entry_size;
    true
  end else false

let change_max_size table max_size =
  table.max_size <- max_size;
  while table.size > max_size do
    evict_one table
  done;
  let capacity = max 128 (max_size lsr 5) in
  if capacity < Array.length table.entries then
    resize table capacity
