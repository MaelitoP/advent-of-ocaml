let parse_line line =
  try
    let parts = String.split_on_char ':' line in
    let test_value = int_of_string (String.trim (List.nth parts 0)) in
    let numbers =
      String.split_on_char ' ' (String.trim (List.nth parts 1))
      |> List.filter (fun s -> s <> "")
      |> List.map int_of_string
    in
    (test_value, numbers)
  with
  | _ -> failwith ("Failed to parse line: " ^ line)

let concat n1 n2 =
  int_of_string (string_of_int n1 ^ string_of_int n2)

(* Recursive function with pruning and memoization *)
let rec evaluate_recursive numbers target index current_result memo =
  if index = List.length numbers then
    current_result = target
  else
    (* Check memoization table *)
    if Hashtbl.mem memo (index, current_result) then
      Hashtbl.find memo (index, current_result)
    else
      let next_num = List.nth numbers index in
      let valid = ref false in
      if current_result + next_num <= target then
        valid := !valid || evaluate_recursive numbers target (index + 1) (current_result + next_num) memo;
      if current_result * next_num <= target then
        valid := !valid || evaluate_recursive numbers target (index + 1) (current_result * next_num) memo;
      let concat_result = concat current_result next_num in
      if concat_result <= target then
        valid := !valid || evaluate_recursive numbers target (index + 1) concat_result memo;
      (* Store result in memoization table *)
      Hashtbl.add memo (index, current_result) !valid;
      !valid

let is_equation_valid test_value numbers =
  if List.length numbers = 0 then false
  else
    let memo = Hashtbl.create 10000 in
    evaluate_recursive numbers test_value 1 (List.hd numbers) memo

let process_file filename =
  let sum = ref 0 in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      if String.trim line <> "" then (
        let test_value, numbers = parse_line line in
        if is_equation_valid test_value numbers then sum := !sum + test_value
      )
    done;
    !sum
  with End_of_file ->
    close_in ic;
    !sum

let () =
  let filename = "src/07/input.txt" in
  let result = process_file filename in
  Printf.printf "Sum of valid test values: %d\n" result

