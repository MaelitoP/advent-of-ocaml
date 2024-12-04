open Lib

let get grid row col =
  let height = Array.length grid in
  let width = String.length grid.(0) in
  if row >= 0 && row < height && col >= 0 && col < width then
    Some grid.(row).[col]
  else None

let is_mas_pattern grid row col dr dc =
  match
    ( get grid row col,
      get grid (row + dr) (col + dc),
      get grid (row + (2 * dr)) (col + (2 * dc)) )
  with
  | Some 'M', Some 'A', Some 'S' -> true
  | Some 'S', Some 'A', Some 'M' -> true
  | _ -> false

let is_xmas grid row col =
  let directions = [ (-1, -1); (-1, 1); (1, -1); (1, 1) ] in
  List.for_all
    (fun (dr, dc) -> is_mas_pattern grid (row - dr) (col - dc) dr dc)
    directions

let count_xmas grid =
  let height = Array.length grid in
  let width = String.length grid.(0) in
  let count = ref 0 in
  for row = 1 to height - 2 do
    for col = 1 to width - 2 do
      if grid.(row).[col] = 'A' && is_xmas grid row col then incr count
    done
  done;
  !count

let () =
  let lines = Utils.read_file "src/04/input.txt" in
  let grid = Array.of_list lines in
  let result = count_xmas grid in
  Printf.printf "Total X-MAS patterns: %d\n" result

