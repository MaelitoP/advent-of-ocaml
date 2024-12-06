open Lib

module PositionSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

type direction = Up | Right | Down | Left

let turn_right = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let move (x, y) = function
  | Up -> (x - 1, y)
  | Right -> (x, y + 1)
  | Down -> (x + 1, y)
  | Left -> (x, y - 1)

let is_within_bounds (x, y) rows cols = x >= 0 && x < rows && y >= 0 && y < cols

let simulate_patrol grid guard_start =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let visited = ref PositionSet.empty in
  let rec loop (x, y) dir =
    if not (is_within_bounds (x, y) rows cols) then
      PositionSet.cardinal !visited
    else (
      visited := PositionSet.add (x, y) !visited;
      let next_pos = move (x, y) dir in
      if
        is_within_bounds next_pos rows cols
        && grid.(fst next_pos).(snd next_pos) = '#'
      then loop (x, y) (turn_right dir) (* Turn right if obstacle ahead *)
      else loop next_pos dir (* Move forward *))
  in
  loop guard_start Up

let find_guard_start grid =
  match
    Array.mapi (fun i row -> (i, Utils.find_index_of_char row '^')) grid
    |> Array.to_list
    |> List.find_opt (fun (_, col_opt) -> Option.is_some col_opt)
  with
  | Some (row, Some col) -> (row, col)
  | _ -> invalid_arg "Guard not found in the grid"

let () =
  let filename = "src/06/input.txt" in
  let grid = Utils.load_file_as_2d_array filename in
  let guard_start = find_guard_start grid in
  let distinct_positions = simulate_patrol grid guard_start in
  Printf.printf "Distinct positions visited: %d\n" distinct_positions
