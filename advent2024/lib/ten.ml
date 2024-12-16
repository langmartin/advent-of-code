module Pos = struct
  type t = int * int

  let make x y = x, y

  let eqv a b =
    fst a == fst b && snd a == snd b

  let plus a b =
    fst a + fst b, snd a + snd b

  let compare (xa, ya) (xb, yb) =
    xa - ya + (xb - yb)
end

module Grid = struct
  let digits s =
    s
    |> String.to_seq
    |> Seq.map (fun c -> (int_of_char c) - 48)
    |> Array.of_seq

  let read file =
    file
    |> One.read_lines
    |> List.map digits
    |> Array.of_list

  let x_len grid = Array.length grid.(0)
  let y_len grid = Array.length grid

  let get grid (x, y) =
    if x >= 0 &&
       y >= 0 &&
       x < x_len grid &&
       y < y_len grid then
      grid.(y).(x)
    else
      -1

  let is_in = Fun.negate (( == ) (-1))

  let cardinal =
    [1, 0; -1, 0; 0, 1; 0, -1]

  let orth grid pos =
    let open List in
    cardinal
    |> map (Pos.plus pos)
    |> filter (fun pos -> is_in (get grid pos))

  let rec range n n_excl =
    if n == n_excl then [] else
      n :: range (n + 1) n_excl

  let positions grid =
    let open List in
    range 0 (x_len grid)
    |> map (fun x ->
        range 0 (y_len grid)
        |> map (fun y -> (x, y))
      )
    |> flatten
end

let find f grid =
  let f = fun pos -> Grid.get grid pos |> f in
  grid
  |> Grid.positions
  |> List.filter f

module Ps = Set.Make(Pos)

(* let rec dfs grid seen pos = *)
(*   let open Grid in *)
(*   let v = get grid pos in *)
(*   let seen = Ps.add pos seen in *)
(*   if v == 9 then *)
(*     [pos] *)
(*   else *)
(*     let open List in *)
(*     orth grid pos *)
(*     |> filter (fun p' -> not (Ps.exists p' seen)) *)
(*     |> filter (fun p' -> get grid p' > v) *)
(*     |> map (fun p' -> p' :: (dfs grid seen p')) *)

let rec trail grid pos =
  let open Grid in
  match get grid pos with
  | 9 -> [[pos]]
  | h ->
    let open List in
    orth grid pos
    |> filter (fun p' -> get grid p' > h)
    |> map (fun p' -> p' :: (trail grid p' |> flatten))

let part1 =
  let grid = "../input/ten-samp.txt" |> Grid.read in
  grid
  |> find (( == ) 0)
  |> List.map (trail grid)
