module Pos = struct
  type t = int * int

  let make x y = x, y

  let eqv a b =
    fst a == fst b && snd a == snd b

  let plus a b =
    fst a + fst b, snd a + snd b

  let compare (xa, ya) (xb, yb) =
    xa - ya + (xb - yb)

  let cardinal =
    [1, 0; -1, 0; 0, 1; 0, -1]
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

  let orth grid pos =
    let open Pos in
    let open List in
    cardinal
    |> map (plus pos)
    |> map (get grid)
    |> filter is_in

  let rec range n n_excl =
    if n == n_excl then [] else
      n :: range (n + 1) n_excl

  let positions grid =
    range 0 (x_len grid)
    |> List.map (fun x ->
        range 0 (y_len grid)
        |> List.map (fun y -> (x, y))
      )
    |> List.flatten
end

let find f grid =
  let open List in
  let f = fun pos -> Grid.get grid pos |> f in
  grid
  |> Grid.positions
  |> List.filter f

let part1 =
  "../input/ten-samp.txt"
  |> Grid.read
  |> find (( == ) 0)
