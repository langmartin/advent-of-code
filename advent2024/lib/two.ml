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

let is_safe_pair op x y =
  op x y && abs (x - y) <= 3

let rec faults op lst =
  match lst with
  | x :: y :: xs ->
    if not (op x y) then
      (x, y) :: faults op (y::xs)
    else
      faults op (y::xs)
  | _ -> []

let find_faults lst =
  match lst with
  | x :: y :: _xs ->
    let op = if x < y then ( < ) else ( > ) in
    let inv = if x < y then ( > ) else ( < ) in
    let fs = faults (is_safe_pair op) lst in
    let fsi = faults (is_safe_pair inv) lst in
    if List.length fs < List.length fsi then
      fs
    else
      fsi
  | _ -> []

let rec is_safe_lp1 op level =
  match level with
  | x :: y :: xs -> op x y && is_safe_lp1 op (y :: xs)
  | _ :: [] -> true
  | [] -> true

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
  | x :: y :: _xs ->
    let op = if x < y then ( < ) else ( > ) in
    is_safe_lp1 (is_safe_pair op) level
    (* is_safe_lp 0 (is_safe_pair op) level *)
  | _ -> false

let is_safe1 level =
  match level with
  | x :: y :: xs ->
    let op = if x < y then ( < ) else ( > ) in
    is_safe_lp 1 (is_safe_pair op) level
    || is_safe_but (y :: xs)
  | _ -> false

let rec take n lst =
  if n == 0 then
    []
  else
    List.hd lst :: take (n - 1) (List.tl lst)

let rec drop n lst =
  if n == 0 then
    lst
  else
    drop (n - 1) (List.tl lst)

let rec any pred lst =
  match lst with
  | [] -> false
  | x :: xs -> pred x || any pred xs

let cycle_one lst =
  lst
  |> List.mapi (fun i _x ->
      List.concat [
        take i lst;
        drop (i + 1) lst
      ])

let rec is_safe level =
  match level with
  | x :: y :: _xs ->
    let op = if x < y then ( < ) else ( > ) in
    is_safe_lp1 (is_safe_pair op) level
    || level
       |> cycle_one
       |> any is_safe
  | _ ->
    false

let unsafe_levels file =
  file
  |> data
  |> List.filter (Fun.negate is_safe)
  |> List.map (fun row -> (find_faults row, row))
  |> List.filter (fun p -> List.length (fst p) == 1)

let safe_levels file =
  file
  |> data
  |> List.filter is_safe
  |> List.length

let samp = safe_levels "../input/two-sample.txt"

let real = safe_levels "../input/two.txt"
