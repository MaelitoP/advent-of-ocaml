(*
    This program determines the number of safe reports in an input file.
    A report is considered safe if:
      - It is strictly increasing or decreasing, and
      - The absolute differences between adjacent levels are in the range [1, 3].

    Example input format:
    $ shuf -n 5 input.txt
    74 78 85 86 88
    30 32 32 29 26 23 20
    82 82 81 84 87 85
    67 66 65 62 60 57 56 49
    66 65 63 60 57 55
*)

open Lib

let parse_line_to_integers line =
  try line |> Str.split (Str.regexp "[ \t]+") |> List.map int_of_string
  with Failure _ ->
    Printf.eprintf "Error: Failed to parse line: '%s'\n%!" line;
    []

let is_strictly_increasing sequence =
  let rec check = function
    | x :: y :: rest -> if x < y then check (y :: rest) else false
    | _ -> true
  in
  check sequence

let is_strictly_decreasing sequence =
  let rec check = function
    | x :: y :: rest -> if x > y then check (y :: rest) else false
    | _ -> true
  in
  check sequence

let are_differences_within_range sequence =
  let rec check = function
    | x :: y :: rest ->
        let difference = abs (x - y) in
        if difference >= 1 && difference <= 3 then check (y :: rest) else false
    | _ -> true
  in
  check sequence

let is_safe report =
  (is_strictly_increasing report || is_strictly_decreasing report)
  && are_differences_within_range report

let () =
  let input_lines = Utils.read_file "./src/02/input.txt" in
  let reports = List.map parse_line_to_integers input_lines in

  let safe_reports_count =
    List.fold_left
      (fun count report -> if is_safe report then count + 1 else count)
      0 reports
  in

  Printf.printf "Number of safe reports: %d\n" safe_reports_count
