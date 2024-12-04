open Lib

let mul_regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))"

let compute_mul_sum text =
  let rec extract_sum pos acc =
    try
      let _ = Str.search_forward mul_regex text pos in
      let x1 = int_of_string (Str.matched_group 1 text) in
      let x2 = int_of_string (Str.matched_group 2 text) in
      let product = x1 * x2 in
      (* Accumulate the product and continue searching *)
      extract_sum (Str.match_end ()) (acc + product)
    with Not_found -> acc (* No more matches, return the accumulated sum *)
  in
  extract_sum 0 0

let () =
  let input_file = "./src/03/input.txt" in
  let text = Utils.read_file_single input_file in

  let total_sum = compute_mul_sum text in

  Printf.printf "Total sum of all mul(x1,x2) products: %d\n" total_sum

