open Lib

module PositionSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

type direction = Up | Right | Down | Left

let turn_right dir =
  match dir with Up -> Right | Right -> Down | Down -> Left | Left -> Up

let move (x, y) dir =
  match dir with
  | Up -> (x - 1, y)
  | Right -> (x, y + 1)
  | Down -> (x + 1, y)
  | Left -> (x, y - 1)

let is_within_bounds (x, y) rows cols = x >= 0 && x < rows && y >= 0 && y < cols

let simulate_with_obstruction grid guard_start obstruction_pos =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let visited = Hashtbl.create 1000 in
  let rec loop (x, y) dir =
    if not (is_within_bounds (x, y) rows cols) then false
      (* Guard leaves grid *)
    else
      let state = ((x, y), dir) in
      if Hashtbl.mem visited state then true (* Cycle detected *)
      else (
        Hashtbl.add visited state true;
        let next_pos = move (x, y) dir in
        if
          is_within_bounds next_pos rows cols
          && (next_pos = obstruction_pos
             || grid.(fst next_pos).(snd next_pos) = '#')
        then loop (x, y) (turn_right dir) (* Turn right if obstacle ahead *)
        else loop next_pos dir (* Move forward *))
  in
  loop guard_start Up

let find_obstruction_positions grid guard_start =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let valid_positions = ref [] in
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
      if (x, y) <> guard_start && grid.(x).(y) = '.' then
        if simulate_with_obstruction grid guard_start (x, y) then
          valid_positions := (x, y) :: !valid_positions
    done
  done;
  !valid_positions

let find_index_opt arr char =
  let rec loop i =
    if i >= Array.length arr then None
    else if arr.(i) = char then Some i
    else loop (i + 1)
  in
  loop 0

let () =
  let filename = "src/06/input.txt" in
  let grid = Utils.load_file_as_2d_array filename in
  let guard_start =
    match
      Array.mapi (fun i row -> (i, find_index_opt row '^')) grid
      |> Array.to_list
      |> List.find_opt (fun (_, col_opt) -> col_opt <> None)
    with
    | Some (row, Some col) -> (row, col)
    | _ -> failwith "Guard not found"
  in
  let obstruction_positions = find_obstruction_positions grid guard_start in
  Printf.printf "Number of valid obstruction positions: %d\n"
    (List.length obstruction_positions);

