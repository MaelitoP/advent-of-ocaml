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

let simulate_with_obstruction grid (start_x, start_y) obstruction =
  let rows, cols = (Array.length grid, Array.length grid.(0)) in
  let visited = Hashtbl.create 1000 in

  let rec patrol (x, y) dir =
    let state = ((x, y), dir) in
    if not (is_within_bounds (x, y) rows cols) then false
    else if Hashtbl.mem visited state then true
    else (
      Hashtbl.add visited state true;
      let next = move (x, y) dir in
      if
        is_within_bounds next rows cols
        && (next = obstruction || grid.(fst next).(snd next) = '#')
      then patrol (x, y) (turn_right dir)
      else patrol next dir)
  in
  patrol (start_x, start_y) Up

let precompute_patrol grid (start_x, start_y) =
  let rows, cols = (Array.length grid, Array.length grid.(0)) in

  let rec patrol path visited (x, y) dir =
    let state = ((x, y), dir) in
    if not (is_within_bounds (x, y) rows cols) then List.rev path
    else if Hashtbl.mem visited state then List.rev path
    else (
      Hashtbl.add visited state true;
      let next = move (x, y) dir in
      if is_within_bounds next rows cols && grid.(fst next).(snd next) = '#'
      then patrol (state :: path) visited (x, y) (turn_right dir)
      else patrol (state :: path) visited next dir)
  in
  patrol [] (Hashtbl.create 1000) (start_x, start_y) Up

let find_obstruction_positions grid guard_start patrol_path =
  let rows, cols = (Array.length grid, Array.length grid.(0)) in
  List.fold_left
    (fun valid_positions ((x, y), dir) ->
      let next_pos = move (x, y) dir in
      if
        is_within_bounds next_pos rows cols
        && grid.(fst next_pos).(snd next_pos) = '.'
        && next_pos <> guard_start
        && simulate_with_obstruction grid guard_start next_pos
      then PositionSet.add next_pos valid_positions
      else valid_positions)
    PositionSet.empty patrol_path

let find_index_of_char arr char =
  let rec loop i =
    if i >= Array.length arr then None
    else if arr.(i) = char then Some i
    else loop (i + 1)
  in
  loop 0

let load_guard_start grid =
  match
    Array.mapi (fun i row -> (i, find_index_of_char row '^')) grid
    |> Array.to_list
    |> List.find_opt (fun (_, col_opt) -> Option.is_some col_opt)
  with
  | Some (row, Some col) -> (row, col)
  | _ -> invalid_arg "Guard not found in the grid"

let () =
  let filename = "src/06/input.txt" in
  let grid = Utils.load_file_as_2d_array filename in
  let guard_start = load_guard_start grid in
  let patrol_path = precompute_patrol grid guard_start in
  let obstruction_positions =
    find_obstruction_positions grid guard_start patrol_path
  in
  Printf.printf "Number of valid obstruction positions: %d\n"
    (PositionSet.cardinal obstruction_positions)

