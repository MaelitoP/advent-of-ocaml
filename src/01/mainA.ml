open Lib

(* Split a string by whitespace (handles multiple spaces or tabs) and parse as two integers *)
let parse_line line =
  try
    let parts = Str.split (Str.regexp "[ \t]+") (String.trim line) in
    match parts with
    | [ col1; col2 ] -> (int_of_string col1, int_of_string col2)
    | _ -> failwith "Invalid line format"
  with Failure _ ->
    Printf.eprintf "Failed to parse line: '%s'\n%!" line;
    raise (Failure "int_of_string")

let () =
  let lines = Utils.read_file "./src/01/input.txt" in

  (* Parse lines into pairs of integers *)
  let parsed_pairs =
    List.filter_map
      (fun line -> try Some (parse_line line) with Failure _ -> None)
      lines
  in

  (* Split into two separate lists *)
  let col1, col2 =
    List.fold_right
      (fun (a, b) (acc1, acc2) -> (a :: acc1, b :: acc2))
      parsed_pairs ([], [])
  in

  (* Sort the lists in ascending order *)
  let sorted_col1 = List.sort compare col1 in
  let sorted_col2 = List.sort compare col2 in

  (* Validate that the two lists have the same size *)
  if List.length sorted_col1 <> List.length sorted_col2 then
    failwith "Error: The two lists must have the same size.";

  (* Combine the lists into pairs *)
  let combined_pairs = List.combine sorted_col1 sorted_col2 in

  (* Compute the absolute distances for each pair *)
  let distances = List.map (fun (x, y) -> abs (x - y)) combined_pairs in

  (* Compute the sum of distances *)
  let sum_of_distances = List.fold_left ( + ) 0 distances in

  (* Print the distances and their sum *)
  Printf.printf "Sum of distances: %d\n" sum_of_distances

