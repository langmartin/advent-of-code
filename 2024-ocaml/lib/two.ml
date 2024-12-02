let read_row line =
  line
  |> String.split_on_char ' '
  |> List.filter (( <> ) "")
  |> List.map int_of_string

let data file =
  file
  |> One.read_lines
  |> List.map read_row

let rec intersperse xs ys =
  match (xs, ys) with
  | (x :: xs, y :: ys) -> x :: y :: intersperse xs ys
  | ([], _ys) -> []
  | (x :: xs, []) -> x :: intersperse xs []

let rec is_safe_lp op level =
  match level with
  | x :: y :: xs -> (op x y) && (abs (x - y) <= 3) && (is_safe_lp op xs)
  | _ :: [] -> true
  | [] -> true

let is_safe level =
  match level with
  | x :: y :: xs ->
    let op = if x < y then ( < ) else ( > ) in
    intersperse level (y :: xs)
    |> is_safe_lp op 
  | _ -> false

let safe_levels file =
  file
  |> data
  |> List.filter is_safe
  |> List.length

let part1_samp = safe_levels "../input/two-sample.txt"

let part1 = safe_levels "../input/two.txt"
