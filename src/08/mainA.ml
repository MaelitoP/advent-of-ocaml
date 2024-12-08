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
  (!antennas, width, height)

let calculate_antinodes antennas width height =
  let antinodes = Hashtbl.create 100 in
  let processed_pairs = Hashtbl.create 100 in

  let is_valid_position x y = x >= 0 && x < width && y >= 0 && y < height in

  let add_antinode (x, y) =
    if is_valid_position x y then Hashtbl.replace antinodes (x, y) ()
  in

  let process_pair (freq1, (x1, y1)) (freq2, (x2, y2)) =
    if freq1 = freq2 && (x1, y1) <> (x2, y2) then
      let pair_key = (min (x1, y1) (x2, y2), max (x1, y1) (x2, y2)) in
      if not (Hashtbl.mem processed_pairs pair_key) then (
        Hashtbl.add processed_pairs pair_key ();
        let dx, dy = (x2 - x1, y2 - y1) in
        add_antinode (x1 - dx, y1 - dy);
        add_antinode (x2 + dx, y2 + dy))
  in

  List.iter
    (fun antenna1 ->
      List.iter (fun antenna2 -> process_pair antenna1 antenna2) antennas)
    antennas;

  Hashtbl.length antinodes

let () =
  let input_file = "src/08/input.txt" in
  let grid = Utils.read_file input_file in
  let antennas, width, height = process_input grid in
  let result = calculate_antinodes antennas width height in
  Printf.printf "Total unique antinodes: %d\n" result
