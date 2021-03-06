module Hashtbl = struct
  include Hashtbl

  let find_opt h key = try Some (find h key) with Not_found -> None
end

module CharSet = Set.Make (Char)
module StringSet = Set.Make (String)

let find_pos names =
  let n = StringSet.cardinal names in
  let rec loop pos =
    if
      StringSet.elements names
      |> List.map (fun name -> name.[pos])
      |> CharSet.of_list
      |> CharSet.cardinal
      |> ( = ) n
    then pos else loop (pos + 1) in
  loop 0

let make_token_table static_table =
  let tbl = Hashtbl.create 60 in
  Array.iter
    (fun (name, _) ->
      let length = String.length name in
        match Hashtbl.find_opt tbl length with
        | Some names ->
          Hashtbl.replace tbl length (StringSet.add name names)
        | None ->
          Hashtbl.add tbl length (StringSet.singleton name))
    static_table;
  tbl

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

let output_lookup_token oc token_table =
  Printf.fprintf oc       "let lookup_token name =\n";
  Printf.fprintf oc       "  match String.length name with\n";
  token_table |> Hashtbl.iter begin fun length names ->
    if StringSet.cardinal names = 1 then
      let name = StringSet.choose names in
      Printf.fprintf oc   "  | %d when String.equal name %S -> Some Token.%a\n"
        length name output_name name;
    else
      let pos = find_pos names in
      Printf.fprintf oc   "  | %d ->\n" length;
      Printf.fprintf oc   "    begin match name.[%d] with\n" pos;
      names |> StringSet.iter begin fun name ->
        Printf.fprintf oc "    | %C when String.equal name %S -> Some Token.%a\n"
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
  let token_table = make_token_table static_table in
  let oc = open_out Sys.argv.(2) in
  Printf.fprintf oc "(* generated by util/gen_static.ml *)\n\n";
  Printf.fprintf oc "let length = 61\n\n";
  output_static_table oc static_table;
  output_tokens oc static_table;
  output_lookup_token oc token_table;
  close_out oc
