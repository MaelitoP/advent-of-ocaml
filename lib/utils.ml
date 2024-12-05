let read_file filename =
  let chan = open_in filename in
  let try_read () = try Some (input_line chan) with End_of_file -> None in
  let rec loop lines =
    match try_read () with
    | Some s -> loop (s :: lines)
    | None ->
        close_in chan;
        List.rev lines
  in
  loop []

let read_file_single filename =
  let chan = open_in filename in
  let content = really_input_string chan (in_channel_length chan) in
  close_in chan;
  content

let split_file_content lines =
  let rec split parts current = function
    | [] -> List.rev (List.rev current :: parts)
    | "" :: rest -> split (List.rev current :: parts) [] rest
    | line :: rest -> split parts (line :: current) rest
  in
  match split [] [] lines with
  | [ rules; updates ] -> (rules, updates)
  | _ -> failwith "Invalid file format"

