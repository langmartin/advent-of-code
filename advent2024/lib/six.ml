module Pos = struct
  type dir = Up | Down | Left | Right
  type t = int * int * dir

  module CharMap = Map.Make(Char)

  let dir_map =
    let open CharMap in
    empty
    |> add '^' Up
    |> add 'v' Down
    |> add '<' Left
    |> add '>' Right

  let dir ch = CharMap.find ch dir_map

  let dxdy dir =
    match dir with
    | Up -> (0, -1) | Down -> (0, 1) | Left -> (-1, 0) | Right -> (1, 0)

  let turn_right pos =
    let (x, y, dir) = pos in
    let dir' = match dir with
      | Up -> Right | Down -> Left | Left -> Up | Right -> Down
    in
    (x, y, dir')

  let adv (x, y, dir) =
    let (dx, dy) = dxdy dir in
    (x + dx, y + dy, dir)

  let is_dir ch =
    CharMap.bindings dir_map |> List.map fst |> List.memq ch

  let make x y ch =
    (x, y, dir ch)

  let coord (x, y, _) = (x, y)
end

let read_grid file =
  file
  |> One.read_lines
  |> List.map (Fun.compose Array.of_seq String.to_seq)
  |> Array.of_list

let get grid x y =
  if x >= 0 &&
     y >= 0 &&
     x < Array.length grid &&
     y < Array.length grid.(0) then
    grid.(y).(x)
  else
    '!'

let find_start grid =
  grid
  |> Array.find_mapi (fun y row ->
      Array.find_mapi (fun x c ->
          if Pos.is_dir c then
            Some (Pos.make x y c)
          else None)
        row)

let rec step_lp grid pos =
  let next = Pos.adv pos in
  let (x', y', _) = next in
  match get grid x' y' with
  | '#' -> pos |> Pos.turn_right |> step_lp grid
  | '!' -> []
  | _ -> next :: step_lp grid next

let step grid pos = pos :: step_lp grid pos

let rec dedup f xs =
  match xs with
  | x :: y :: xs -> if f x y then
      x :: dedup f xs
    else
      x :: dedup f (y :: xs)
  | xs -> xs

let eq (x, y) (x', y') = x == x' && y == y'

let p1 file =
  let g = file |> read_grid in
  (match g |> find_start with
  | None -> []
  | Some pos -> step g pos)
  |> List.map Pos.coord
  |> List.sort compare
  |> dedup eq
  |> List.length

let samp1 = p1 "../input/six-samp.txt"

let part1 = p1 "../input/six.txt"
