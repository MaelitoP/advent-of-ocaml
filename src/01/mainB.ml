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

(* Count occurrences of each element in a list *)
let count_occurrences lst =
  List.fold_left
    (fun acc value ->
      let count = try List.assoc value acc with Not_found -> 0 in
      (value, count + 1) :: List.remove_assoc value acc)
    [] lst

(* Create a dictionary for occurrences (for easier lookup) *)
let create_occurrence_dict lst =
  List.fold_left (fun acc (k, v) -> (k, v) :: acc) [] lst

(* Parse the lines from the file and compute the result *)
let compute_sum file_path =
  let lines = Utils.read_file file_path in

  (* Parse lines into pairs of integers *)
  let parsed_pairs =
    List.filter_map
      (fun line -> try Some (parse_line line) with Failure _ -> None)
      lines
  in

  (* Split parsed pairs into two separate lists *)
  let col1, col2 = List.split parsed_pairs in

  (* Count occurrences of values in col2 and create a lookup dictionary *)
  let occurrences_dict = count_occurrences col2 |> create_occurrence_dict in

  (* Compute the result: sum of list1Value * occurrencesOfList2.get(list1Value) *)
  let results =
    List.map
      (fun list1Value ->
        try
          let occurrences = List.assoc list1Value occurrences_dict in
          list1Value * occurrences
        with Not_found -> 0)
      col1
  in

  (* Return the sum of results *)
  List.fold_left ( + ) 0 results

let () =
  let sum = compute_sum "./src/01/input.txt" in
  Printf.printf "Sum of results: %d\n" sum

