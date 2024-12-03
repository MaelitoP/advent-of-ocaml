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

(* Validate that two lists have the same length *)
let validate_lists_length lst1 lst2 =
  if List.length lst1 <> List.length lst2 then
    failwith "Error: The two lists must have the same size."

(* Sort and pair two lists of integers *)
let sort_and_combine_lists lst1 lst2 =
  let sorted_lst1 = List.sort compare lst1 in
  let sorted_lst2 = List.sort compare lst2 in
  validate_lists_length sorted_lst1 sorted_lst2;
  List.combine sorted_lst1 sorted_lst2

(* Compute the absolute distances for a list of pairs *)
let compute_distances pairs = List.map (fun (x, y) -> abs (x - y)) pairs

(* Parse the file, compute and return the sum of distances *)
let compute_sum_of_distances file_path =
  let lines = Utils.read_file file_path in

  (* Parse lines into pairs of integers *)
  let parsed_pairs =
    List.filter_map
      (fun line -> try Some (parse_line line) with Failure _ -> None)
      lines
  in

  (* Split parsed pairs into two separate lists *)
  let col1, col2 = List.split parsed_pairs in

  (* Sort the lists, combine them into pairs, and compute distances *)
  let combined_pairs = sort_and_combine_lists col1 col2 in
  let distances = compute_distances combined_pairs in

  (* Return the sum of distances *)
  List.fold_left ( + ) 0 distances

let () =
  let sum = compute_sum_of_distances "./src/01/input.txt" in
  Printf.printf "Sum of distances: %d\n" sum

