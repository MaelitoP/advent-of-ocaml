open Lib

(* Parse a single line into a pair of integers *)
let parse_line line =
  try
    let parts = Str.split (Str.regexp "[ \t]+") (String.trim line) in
    match parts with
    | [ col1; col2 ] -> (int_of_string col1, int_of_string col2)
    | _ -> failwith "Invalid line format"
  with Failure _ ->
    Printf.eprintf "Failed to parse line: '%s'\n%!" line;
    raise (Failure "int_of_string")

let validate_lists_length list1 list2 =
  if List.length list1 <> List.length list2 then
    failwith "Error: The two lists must have the same size."

let sort_and_combine_lists list1 list2 =
  let sorted_list1 = List.sort compare list1 in
  let sorted_list2 = List.sort compare list2 in
  validate_lists_length sorted_list1 sorted_list2;
  List.combine sorted_list1 sorted_list2

let compute_distances pairs = List.map (fun (x, y) -> abs (x - y)) pairs

let compute_sum_of_distances file_path =
  let lines = Utils.read_file file_path in

  let parsed_pairs =
    List.filter_map
      (fun line -> try Some (parse_line line) with Failure _ -> None)
      lines
  in

  let col1, col2 = List.split parsed_pairs in

  let combined_pairs = sort_and_combine_lists col1 col2 in
  let distances = compute_distances combined_pairs in

  List.fold_left ( + ) 0 distances

let () =
  let sum = compute_sum_of_distances "./src/01/input.txt" in
  Printf.printf "Sum of distances: %d\n" sum

