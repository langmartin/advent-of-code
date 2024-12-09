let wrap x = [x]

let rec ps len xs =
  let open List in
  if len < 1 then raise (Invalid_argument "at least 1") else
  if len == 1 then List.map wrap xs else
    let ps' = ps (len - 1) xs in
    xs
    |> map (fun x -> map (cons x) ps')
    |> concat

let is_empty s = String.length s == 0

let read_args space_sep_ints =
  space_sep_ints
  |> String.split_on_char ' '
  |> List.filter (Fun.negate is_empty)
  |> List.map int_of_string

let read_ln line =
  match String.split_on_char ':' line with
  | ans :: argstr :: [] ->
    (int_of_string ans, read_args argstr)
  | _ -> raise (Invalid_argument "parsing")

let ops = [( + ); ( * )]

let exc ns ops =
  List.fold_left2 (fun tot n op -> op tot n)
    (List.hd ns)
    (List.tl ns)
    ops

let go1 ops (ans, args) =
  let open List in
  let ps = ps (List.length args - 1) ops in
  let found = ps |> map (exc args) |> filter (( == ) ans) in
  if is_empty found then
    0
  else
    ans

let part1 =
  let go = go1 ops in
  "../input/seven.txt"
  |> One.read_lines
  |> List.map read_ln
  |> List.map go
  |> One.sum

(* part 2 *)

let ncat n m =
  [n; m]
  |> List.map Int.to_string
  |> String.concat ""
  |> int_of_string

let ops2 = [( + ); ( * ); ncat]

let part2 =
  let go = go1 ops2 in
  "../input/seven.txt"
  |> One.read_lines
  |> List.map read_ln
  |> List.map go
  |> One.sum

