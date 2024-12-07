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
  with _ -> failwith ("Failed to parse line: " ^ line)

let rec evaluate_expression numbers operators =
  match (numbers, operators) with
  | [ n ], [] -> n
  | n1 :: n2 :: rest_nums, op :: rest_ops ->
      let partial_result =
        match op with
        | '+' -> n1 + n2
        | '*' -> n1 * n2
        | '|' -> int_of_string (string_of_int n1 ^ string_of_int n2)
        | _ -> failwith "Invalid operator"
      in
      evaluate_expression (partial_result :: rest_nums) rest_ops
  | _ -> failwith "Invalid numbers or operators"

let rec generate_operator_combinations len =
  if len = 0 then [ [] ]
  else
    let shorter_combinations = generate_operator_combinations (len - 1) in
    List.concat
      [
        List.map (fun ops -> '+' :: ops) shorter_combinations;
        List.map (fun ops -> '*' :: ops) shorter_combinations;
        List.map (fun ops -> '|' :: ops) shorter_combinations;
      ]

let is_equation_valid test_value numbers =
  let num_operators = List.length numbers - 1 in
  let operator_combinations = generate_operator_combinations num_operators in
  List.exists
    (fun operators -> evaluate_expression numbers operators = test_value)
    operator_combinations

let process_file filename =
  let sum = ref 0 in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      if String.trim line <> "" then
        let test_value, numbers = parse_line line in
        if is_equation_valid test_value numbers then sum := !sum + test_value
    done;
    !sum
  with End_of_file ->
    close_in ic;
    !sum

let () =
  let filename = "src/07/input.txt" in
  let result = process_file filename in
  Printf.printf "Sum of valid test values: %d\n" result
