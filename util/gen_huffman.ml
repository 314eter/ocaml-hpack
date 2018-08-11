type node = {
  mutable id : int;
  mutable accept : bool;
  left : child;
  right : child;
  transitions : (int option * bool * char option) array;
} and child = Node of node | Symbol of char | Missing

let make_node ?(left=Missing) ?(right=Missing) () =
  { id = 0; accept = false; left; right; transitions = Array.make 16 (None, false, None) }

let rec add_symbol tree symbol = function
  | [] -> Symbol symbol
  | false :: bits ->
    begin match tree with
    | Missing -> Node (make_node ~left:(add_symbol tree symbol bits) ())
    | Node node -> Node {node with left = add_symbol node.left symbol bits}
    | Symbol _ -> failwith "add_symbol"
    end
  | true :: bits ->
    begin match tree with
    | Missing -> Node (make_node ~right:(add_symbol tree symbol bits) ())
    | Node node -> Node {node with right = add_symbol node.right symbol bits}
    | Symbol _ -> failwith "add_symbol"
    end

let rec set_ids tree eos next_id =
  match tree with
  | Node node ->
    node.id <- next_id;
    if eos < 8 then node.accept <- true;
    next_id + 1
    |> set_ids node.left 8
    |> set_ids node.right (eos + 1)
  | _ -> next_id

let rec traverse root transitions failed symbol node remaining i =
  let (failed, node, symbol) =
    match node with
    | Symbol symbol -> (failed, root, Some symbol)
    | Node node -> (failed, node, symbol)
    | Missing -> (true, root, None) in
  if remaining = 0 then begin
    transitions.(i) <-
      if failed then (None, false, None)
      else (Some node.id, node.accept, symbol);
    i + 1
  end else
    traverse root transitions failed symbol node.left (remaining - 1) i
    |> traverse root transitions failed symbol node.right (remaining - 1)

let rec make_transitions root = function
  | Node node ->
    let i = traverse root node.transitions false None (Node node) 4 0 in
    assert (i = 16);
    make_transitions root node.left;
    make_transitions root node.right;
  | _ -> ()

let output_encode_table oc encode_table =
  Printf.fprintf oc "let encode_table = [|\n";
  Array.iteri begin fun i (code, length) ->
    Printf.fprintf oc "  (* %3d *) (0x%x, %d);\n" i code length
  end encode_table;
  Printf.fprintf oc "|]\n\n"

let output_transition oc (id, accept, symbol) =
  let output_option output oc = function
    | Some x -> Printf.fprintf oc "Some %a" output x
    | None -> Printf.fprintf oc "None" in
  let output_int oc = Printf.fprintf oc "%d" in
  let output_bool oc b = Printf.fprintf oc (if b then "true" else "false") in
  let output_char oc = Printf.fprintf oc "%C" in
  Printf.fprintf oc "(%a, %a, %a)"
    (output_option output_int) id
    output_bool accept
    (output_option output_char) symbol

let output_decode_table oc tree =
  Printf.fprintf oc "let decode_table = [|\n";
  let rec loop tree i =
    match tree with
    | Node node ->
      assert (node.id = i);
      Printf.fprintf oc "\n  (* -- %d -- *)\n" node.id;
      Array.iteri begin fun i transition ->
        Printf.fprintf oc "  (* %3d *) %a;\n" i output_transition transition
      end node.transitions;
      i + 1 |> loop node.left |> loop node.right
    | _ -> i in
  let i = loop tree 0 in
  assert (i = 256);
  Printf.fprintf oc "|]\n"

let bits_of_string s =
  let rec aux i =
    if i < String.length s then
      match s.[i] with
      | '|' -> aux (i + 1)
      | '0' -> false :: aux (i + 1)
      | '1' -> true :: aux (i + 1)
      | _ -> failwith "bits_of_string"
    else [] in
  aux 0

let () =
  let ic = Scanf.Scanning.from_file Sys.argv.(1) in
  let encode_table = Array.make 256 (0, 0) in
  let rec loop tree i =
    if i < 256 then
      Scanf.bscanf ic "%_c%_c%_c ( %d ) %s %x [ %d ]\n" @@ fun _i s code length ->
      assert (i = _i);
      encode_table.(i) <- (code, length);
      let tree = add_symbol tree (char_of_int i) (bits_of_string s) in
      loop tree (i + 1)
    else
      let ids = set_ids tree 0 0 in
      assert (ids = 256); tree in
  let tree = loop Missing 0 in
  let root =
    match tree with
    | Node node -> node
    | _ -> failwith "empty tree" in
  make_transitions root tree;
  let oc = open_out Sys.argv.(2) in
  Printf.fprintf oc "(* generated by util/gen_huffman.ml *)\n\n";
  output_encode_table oc encode_table;
  output_decode_table oc tree;
  close_out oc
