open Lib

let process_input grid =
  let antennas = ref [] in
  let height = List.length grid in
  let width = String.length (List.hd grid) in
  List.iteri
    (fun y row ->
      String.iteri
        (fun x cell ->
          if cell <> '.' then antennas := (cell, (x, y)) :: !antennas)
        row)
    grid;
  let sorted_antennas =
    List.sort (fun (_, pos1) (_, pos2) -> compare pos1 pos2) !antennas
  in
  (sorted_antennas, width, height)

module CoordSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let calculate_antinodes antennas width height =
  let antinodes = ref CoordSet.empty in

  let is_valid_position x y = x >= 0 && x < width && y >= 0 && y < height in

  let add_antinode (x, y) =
    if is_valid_position x y then antinodes := CoordSet.add (x, y) !antinodes
  in

  let rec extend_antinode (x, y) (dx, dy) =
    let new_x, new_y = (x + dx, y + dy) in
    if is_valid_position new_x new_y then (
      add_antinode (new_x, new_y);
      extend_antinode (new_x, new_y) (dx, dy))
  in

  let process_pair (freq1, (x1, y1)) (freq2, (x2, y2)) =
    if freq1 = freq2 && (x1, y1) <> (x2, y2) then (
      let dx, dy = (x2 - x1, y2 - y1) in
      add_antinode (x1, y1);
      add_antinode (x2, y2);
      extend_antinode (x1, y1) (-dx, -dy);
      extend_antinode (x2, y2) (dx, dy))
  in

  List.iteri
    (fun i antenna1 ->
      List.iteri
        (fun j antenna2 -> if j > i then process_pair antenna1 antenna2)
        antennas)
    antennas;

  CoordSet.cardinal !antinodes

let () =
  let input_file = "src/08/input.txt" in
  let grid = Utils.read_file input_file in
  let antennas, width, height = process_input grid in
  let result = calculate_antinodes antennas width height in
  Printf.printf "Total unique antinodes: %d\n" result

