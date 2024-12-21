module Pos = Ten.Pos

let is_map_ln = Five.has '#'

let array_of_ln s = s |> String.to_seq |> Array.of_seq

let read_map file =
  file
  |> One.read_lines
  |> Five.take_while is_map_ln
  |> List.map array_of_ln
  |> Array.of_list

let read_moves file =
  file
  |> One.read_lines
  |> Five.drop_while (fun s -> s == "" || is_map_ln s)
  |> List.map (fun s -> s |> String.to_seq |> List.of_seq)
  |> List.concat

let move c =
  match c with
  | '^' -> 0, 1
  | '>' -> 1, 0
  | '<' -> -1, 0
  | 'v' -> 0, -1
  | _ -> raise (Invalid_argument "")

let x_len grid = Array.length grid.(0)
let y_len grid = Array.length grid
let is_edge = (( == ) '#')

let get grid (x, y) =
  if x >= 0 &&
     y >= 0 &&
     x < x_len grid &&
     y < y_len grid then
    grid.(y).(x)
  else
    '!'

let positions grid =
  let open List in
  let open Ten.Grid in
  range 0 (x_len grid)
  |> map (fun x ->
      range 0 (y_len grid)
      |> map (fun y -> (x, y))
    )
  |> flatten

let find f grid =
  let f = fun pos -> get grid pos |> f in
  grid
  |> positions
  |> List.filter f

let find_dot grid pos mv =
  let rec lp cur n =
    let cur = Pos.plus mv cur in
    match get grid cur with
    | '#' -> -1
    | '.' -> n
    | _ -> lp cur (n + 1)
  in
  lp pos 0

let rec over pos n mv =
  if n == 0 then pos else over (Pos.plus pos mv) (n - 1) mv

let put pos c grid =
  let (x, y) = pos in
  Array.set grid.(y) x c;
  grid

let push grid pos mv =
  match find_dot grid pos mv with
  | -1 -> grid
  | n ->
    grid
    |> put (over pos n mv) 'O'
    |> put pos '.'

let step (grid, pos) m =
  let mv = move m in
  let next = Pos.plus pos mv in
  match get grid next with
  | '#' -> grid, pos
  | '.' -> grid, next
  | 'O' ->
    let g' = push grid pos mv in
    if get g' next == '.' then
      g', next
    else
      grid, pos
  | c -> raise (Invalid_argument (String.make 1 c))

let walk file =
  let grid = file |> read_map in
  (* let moves = file |> read_moves in *)
  let start = find (( == ) '@') grid |> List.hd in
  file
  |> read_moves
  |> List.fold_left step (grid, start)

let part1 () = walk "../input/fifteen-samp.txt"
