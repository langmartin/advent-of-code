let read_row line =
  line
  |> String.split_on_char ' '
  |> List.filter (( <> ) "")
  |> List.map int_of_string

let data file =
  file
  |> One.read_lines
  |> List.map read_row

(*
 let rec intersperse xs ys =
  match (xs, ys) with
  | (x :: xs, y :: ys) -> x :: y :: intersperse xs ys
  | ([], _ys) -> []
   | (x :: xs, []) -> x :: intersperse xs []
 *)

let rec is_safe_lp1 op level =
  match level with
  | x :: y :: xs -> op x y && is_safe_lp1 op (y :: xs)
  | _ :: [] -> true
  | [] -> true

let is_safe_pair op x y =
  op x y && abs (x - y) <= 3

let rec is_safe_lp tolerance op level =
  match level with
  | x :: y :: z :: xs ->
    if op x y then
      is_safe_lp tolerance op (y :: z :: xs)
    else
      tolerance > 0
      && op x z
      && is_safe_lp (tolerance - 1) op (z :: xs)
  | x :: y :: [] ->
    if tolerance > 0 then true else op x y
  | _ :: [] -> true
  | [] -> true

let is_safe_but level =
  match level with
  | x :: y :: xs ->
    let op = if x < y then ( < ) else ( > ) in
    is_safe_lp1 (is_safe_pair op) level
    (* is_safe_lp 0 (is_safe_pair op) level *)
  | _ -> false

let is_safe level =
  match level with
  | x :: y :: xs ->
    let op = if x < y then ( < ) else ( > ) in
    is_safe_lp 1 (is_safe_pair op) level
    || is_safe_but (y :: xs)
  | _ -> false

let unsafe_levels file =
  file
  |> data
  |> List.filter (Fun.negate is_safe)

let safe_levels file =
  file
  |> data
  |> List.filter is_safe
  |> List.length

let samp = safe_levels "../input/two-sample.txt"

let real = safe_levels "../input/two.txt"
