open Lib

let target_word = "XMAS"

(* Directions for word search: right, down, diagonals *)
let search_directions =
  [ (0, 1); (1, 0); (1, 1); (1, -1); (0, -1); (-1, 0); (-1, -1); (-1, 1) ]

(* Check if a word can be formed starting at (start_row, start_col) in direction (row_step, col_step) *)
let can_form_word grid target_word start_row start_col row_step col_step =
  let num_rows = List.length grid in
  let num_cols = String.length (List.hd grid) in
  let word_length = String.length target_word in
  let rec aux word_index =
    if word_index = word_length then true
    else
      let current_row = start_row + (word_index * row_step) in
      let current_col = start_col + (word_index * col_step) in
      if
        current_row < 0 || current_col < 0 || current_row >= num_rows
        || current_col >= num_cols
      then false
      else
        match List.nth_opt grid current_row with
        | Some row
          when String.get row current_col = String.get target_word word_index ->
            aux (word_index + 1)
        | _ -> false
  in
  aux 0

(* Count occurrences of the target word in all possible directions *)
let count_word_occurrences grid target_word =
  let num_rows = List.length grid in
  let num_cols = String.length (List.hd grid) in
  let count_occurrences_at_position row col =
    List.fold_left
      (fun acc (row_step, col_step) ->
        if can_form_word grid target_word row col row_step col_step then acc + 1
        else acc)
      0 search_directions
  in
  let count = ref 0 in
  for row = 0 to num_rows - 1 do
    for col = 0 to num_cols - 1 do
      count := !count + count_occurrences_at_position row col
    done
  done;
  !count

let () =
  let grid = Utils.read_file "src/04/input.txt" in
  let total_occurrences = count_word_occurrences grid target_word in
  Printf.printf "Total occurrences of '%s': %d\n" target_word total_occurrences

