open Lib

(* Parse a single line into a pair of integers *)
let parse_line line =
  try
    let parts = Str.split (Str.regexp "[ \t]+") (String.trim line) in
    match parts with
    | [col1; col2] -> (int_of_string col1, int_of_string col2)
    | _ -> failwith "Invalid line format"
  with Failure _ ->
    Printf.eprintf "Failed to parse line: '%s'\n%!" line;
    raise (Failure "int_of_string")

let count_occurrences lst =
  let table = Hashtbl.create (List.length lst) in
  List.iter (fun value ->
    let count = try Hashtbl.find table value with Not_found -> 0 in
    Hashtbl.replace table value (count + 1)
  ) lst;
  table

let compute_sum file_path =
  let lines = Utils.read_file file_path in

  (* Parse lines into pairs of integers *)
  let parsed_pairs =
    List.filter_map
      (fun line -> try Some (parse_line line) with Failure _ -> None)
      lines
  in

  let col1, col2 = List.split parsed_pairs in

  let occurrences_in_col2 = count_occurrences col2 in

  let results =
    List.map
      (fun col1_value ->
        try
          let occurrences = Hashtbl.find occurrences_in_col2 col1_value in
          col1_value * occurrences
        with Not_found -> 0)
      col1
  in

  List.fold_left (+) 0 results

let () =
  let sum = compute_sum "./src/01/input.txt" in
  Printf.printf "Sum of results: %d\n" sum

