module IntMap = Map.Make (struct
    type t = int
    let compare = compare
  end)

module StringSet = Set.Make (String)

module CharSet = Set.Make (Char)


let make_token_map =
  Array.fold_left begin fun map (name, _) ->
    let length = String.length name in
    IntMap.update length begin function
    | Some names -> Some (StringSet.add name names)
    | None -> Some (StringSet.singleton name)
    end map
  end IntMap.empty

let find_pos names =
  let n = StringSet.cardinal names in
  let rec loop pos =
    if
      StringSet.to_seq names
      |> Seq.map (fun name -> name.[pos])
      |> CharSet.of_seq
      |> CharSet.cardinal
      |> ( = ) n
    then pos else loop (pos + 1) in
  loop 0


let output_name oc =
  String.iter begin function
  | 'a' .. 'z' as c -> output_char oc c
  | _ -> output_char oc '_'
  end

let output_static_table oc static_table =
  Printf.fprintf oc   "let table = [|\n";
  static_table |> Array.iteri begin fun i (name, value) ->
    Printf.fprintf oc "  (* %2d *) (%S, %S);\n" (i + 1) name value
  end;
  Printf.fprintf oc   "|]\n\n"

let output_tokens oc static_table =
  Printf.fprintf oc     "module Token = struct\n";
  static_table |> Array.fold_left begin fun (i, prev) (name, _) ->
    if name <> prev then
      Printf.fprintf oc "  let %a = %d\n" output_name name i;
    (i + 1, name)
  end (0, "") |> ignore;
  Printf.fprintf oc     "end\n";
  Printf.fprintf oc "\n"

let output_lookup_token oc token_map =
  Printf.fprintf oc       "let lookup_token name =\n";
  Printf.fprintf oc       "  match String.length name with\n";
  token_map |> IntMap.iter begin fun length names ->
    if StringSet.cardinal names = 1 then
      let name = StringSet.choose names in
      Printf.fprintf oc   "  | %d when name = %S -> Some Token.%a\n"
        length name output_name name;
    else
      let pos = find_pos names in
      Printf.fprintf oc   "  | %d ->\n" length;
      Printf.fprintf oc   "    begin match name.[%d] with\n" pos;
      names |> StringSet.iter begin fun name ->
        Printf.fprintf oc "    | %C when name = %S -> Some Token.%a\n"
          name.[pos] name output_name name;
      end;
      Printf.fprintf oc   "    | _ -> None\n";
      Printf.fprintf oc   "    end\n";
  end;
  Printf.fprintf oc       "  | _ -> None\n"

let () =
  let ic = open_in Sys.argv.(1) in
  let static_table = Array.init 61 @@ fun i ->
    let line = input_line ic in
    match String.split_on_char '\t' line with
    | [s; name] when int_of_string s = i + 1 -> (name, "")
    | [s; name; value] when int_of_string s = i + 1 -> (name, value)
    | _ -> assert false in
  let token_map = make_token_map static_table in
  let oc = open_out Sys.argv.(2) in
  Printf.fprintf oc "(* generated by util/gen_static.ml *)\n\n";
  Printf.fprintf oc "let size = 61\n\n";
  output_static_table oc static_table;
  output_tokens oc static_table;
  output_lookup_token oc token_map;
  close_out oc
