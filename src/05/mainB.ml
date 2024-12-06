open Lib

let index_of lst x =
  let rec aux idx = function
    | [] -> raise Not_found
    | y :: ys -> if y = x then idx else aux (idx + 1) ys
  in
  aux 0 lst

let build_graph edges =
  let graph = Hashtbl.create 100 in
  List.iter
    (fun (x, y) ->
      if Hashtbl.mem graph x then
        Hashtbl.replace graph x (y :: Hashtbl.find graph x)
      else Hashtbl.add graph x [ y ];
      if not (Hashtbl.mem graph y) then Hashtbl.add graph y [])
    edges;
  graph

let detect_cycle graph =
  let visited = Hashtbl.create 100 in
  let rec_stack = Hashtbl.create 100 in

  let rec dfs path node =
    if Hashtbl.find_opt rec_stack node = Some true then
      Some (List.rev (node :: path)) (* Cycle detected *)
    else if Hashtbl.find_opt visited node = Some true then None
    else (
      Hashtbl.replace visited node true;
      Hashtbl.replace rec_stack node true;
      let neighbors = Hashtbl.find graph node in
      let rec explore = function
        | [] -> None
        | n :: rest -> (
            match dfs (node :: path) n with
            | Some cycle -> Some cycle
            | None -> explore rest)
      in
      let result = explore neighbors in
      Hashtbl.replace rec_stack node false;
      result)
  in

  let rec find_cycle nodes =
    match nodes with
    | [] -> None
    | node :: rest -> (
        match dfs [] node with
        | Some cycle -> Some cycle
        | None -> find_cycle rest)
  in
  find_cycle (Hashtbl.fold (fun k _ acc -> k :: acc) graph [])

(* Function for topological sorting (Kahn's algorithm) *)
let topological_sort graph =
  let in_degree = Hashtbl.create 100 in
  Hashtbl.iter
    (fun _ neighbors ->
      List.iter
        (fun neighbor ->
          Hashtbl.replace in_degree neighbor
            ((Hashtbl.find_opt in_degree neighbor |> Option.value ~default:0)
            + 1))
        neighbors)
    graph;

  let queue = Queue.create () in
  Hashtbl.iter
    (fun node _ ->
      if Hashtbl.find_opt in_degree node |> Option.value ~default:0 = 0 then
        Queue.add node queue)
    graph;

  let sorted = ref [] in
  while not (Queue.is_empty queue) do
    let node = Queue.pop queue in
    sorted := node :: !sorted;
    List.iter
      (fun neighbor ->
        let deg = Hashtbl.find in_degree neighbor - 1 in
        Hashtbl.replace in_degree neighbor deg;
        if deg = 0 then Queue.add neighbor queue)
      (Hashtbl.find graph node)
  done;
  if List.length !sorted = Hashtbl.length graph then Some (List.rev !sorted)
  else None (* Cycle detected *)

(* Function to check if an update is in the correct order *)
let is_correct_order update topo_order =
  let pos_map = Hashtbl.create 100 in
  List.iteri (fun idx x -> Hashtbl.add pos_map x idx) topo_order;
  let rec check = function
    | [] | [ _ ] -> true
    | x :: (y :: _ as rest) ->
        if Hashtbl.find pos_map x > Hashtbl.find pos_map y then false
        else check rest
  in
  check update

let middle_page update =
  let len = List.length update in
  List.nth update (len / 2)

let process_updates edges updates =
  let edges =
    List.map (fun s -> Scanf.sscanf s "%d|%d" (fun x y -> (x, y))) edges
  in
  let updates =
    List.map
      (fun s -> String.split_on_char ',' s |> List.map int_of_string)
      updates
  in
  let incorrectly_ordered_updates = ref [] in

  List.iter
    (fun update ->
      let subgraph_edges =
        List.filter (fun (x, y) -> List.mem x update && List.mem y update) edges
      in
      let graph = build_graph subgraph_edges in
      match detect_cycle graph with
      | Some _ -> ()
      | None -> (
          match topological_sort graph with
          | Some topo_order ->
              if not (is_correct_order update topo_order) then
                let reordered_update =
                  List.sort
                    (fun x y ->
                      compare (index_of topo_order x) (index_of topo_order y))
                    update
                in
                incorrectly_ordered_updates :=
                  reordered_update :: !incorrectly_ordered_updates
          | None -> ()))
    updates;

  List.fold_left
    (fun acc update -> acc + middle_page update)
    0
    !incorrectly_ordered_updates

let () =
  let lines = Utils.read_file "src/05/input.txt" in
  let edges, updates = Utils.split_file_content lines in
  let result = process_updates edges updates in
  Printf.printf "Sum of middle pages for incorrectly ordered updates: %d\n"
    result
