let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let read_data() =
  "../input/one.txt"
  |> read_lines
  |> List.map (fun line ->
      let four = String.split_on_char ' ' line in
      let left = List.hd four |> int_of_string in
      let right = List.nth four 3 |> int_of_string in
      (left, right))

let data = read_data()

let left =
  data
  |> List.map fst
  |> List.sort compare

let right =
  data
  |> List.map snd
  |> List.sort compare

let sum lst = List.fold_left ( + ) 0 lst

let part1 =
  List.map2 (fun l r ->
      abs (l - r)
    )
    left
    right
  |> sum

let sim_score lst n =
  n * begin
    lst
    |> List.filter (fun m -> n == m)
    |> List.length
  end

(* let left = [3; 4; 2; 1; 3; 3;] *)
(* let right = [4; 3; 5; 3; 9; 3] *)

let part2 =
  left |> List.map (sim_score right) |> sum
