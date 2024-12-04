open Lib

let process_file filename =
  let input = Utils.read_file_single filename in
  let mul_regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let toggle_do_regex = Str.regexp "do()" in
  let toggle_dont_regex = Str.regexp "don't()" in

  let rec extract_and_sum acc start_idx enabled =
    if start_idx >= String.length input then acc
    else if Str.string_match mul_regex input start_idx then
      let next_idx = Str.match_end () in
      if enabled then
        let x1 = int_of_string (Str.matched_group 1 input) in
        let x2 = int_of_string (Str.matched_group 2 input) in
        let product = x1 * x2 in
        extract_and_sum (acc + product) next_idx enabled
      else extract_and_sum acc (Str.match_end ()) enabled
    else if Str.string_match toggle_do_regex input start_idx then
      extract_and_sum acc (Str.match_end ()) true
    else if Str.string_match toggle_dont_regex input start_idx then
      extract_and_sum acc (Str.match_end ()) false
    else extract_and_sum acc (start_idx + 1) enabled
  in

  extract_and_sum 0 0 true

let () =
  let filename = "src/03/input.txt" in
  let result = process_file filename in
  Printf.printf "Total sum of all enabled mul(x1,x2) products: %d\n" result

