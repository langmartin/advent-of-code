let data =
  "../input/four.txt"
  |> One.read_lines
  |> List.map (Fun.compose Array.of_seq String.to_seq)
  |> Array.of_list

let max_x data = Array.length data.(0) - 1
let max_y data = Array.length data - 1

let xmas = "XMAS"

let next_c c =
  let i = String.index xmas c in
  if i < (String.length xmas - 1) then
    Some (String.get xmas (i + 1))
  else
    None

let approach x xx =
  if xx > x then
    x + 1
  else if xx < x then
    x - 1
  else
    x

let adv src trg =
  let (x, y) = src in
  let (xx, yy) = trg in
  (approach x xx, approach y yy)

let get x y data =
  if x >= 0 &&
     y >= 0 &&
     x <= max_x data &&
     y <= max_y data then
    Some (data.(x).(y))
  else
    None

let rec marks_the_spot_lp next_c src trg c data =
  let (x, y) = src in
  match get x y data with
  | None -> false
  | Some d -> d == c && begin
      match next_c c with
      | None -> true
      | Some c -> marks_the_spot_lp next_c (adv src trg) trg c data
    end

let marks_the_spot src trg c data =
  marks_the_spot_lp next_c src trg c data

let look_around1 src data =
  let f = fun trg ->
    if marks_the_spot src trg 'X' data then
      Some (src, trg)
    else
      None
  in
  let dt = String.length xmas in
  let (x, y) = src in
  [
    (x + dt, y);                (* right *)
    (x - dt, y);                (* left *)

    (x, y + dt);                (* down *)
    (x, y - dt);                (* up *)

    (x + dt, y + dt);           (* four diagonals *)
    (x + dt, y - dt);
    (x - dt, y + dt);
    (x - dt, y - dt);
  ]
  |> List.map f

(* sets are more complicated than I'd hoped *)
let is_ms a b =
  (a == 'M' && b == 'S') || (a == 'S' && b == 'M')

let gp data x y =
  let max_x = max_x data in
  let max_y = max_y data in
  if 0 <= x && x <= max_x && 0 <= y && y <= max_y then
    data.(x).(y)
  else
    '!'

let look_around2 src data =
  let wrap v = [v] in
  let gp = gp data in
  let (x, y) = src in
  begin
    if is_ms (gp (x + 1) (y + 1)) (gp (x - 1) (y - 1)) &&
       is_ms (gp (x + 1) (y - 1)) (gp (x - 1) (y + 1)) then
      Some (x, y)
    else
      None
  end
  |> wrap

let search is_x look_around data =
  let rec lp x y =
    if x > max_x data then
      []
    else if y > max_y data then
      lp (x + 1) 0
    else
      let c = data.(x).(y) in
      if is_x c then
        look_around (x, y) data :: lp x (y + 1)
      else
        lp x (y + 1)
  in
  lp 0 0
  |> List.flatten
  |> List.filter Option.is_some
  |> List.map Option.get

let part1 = data |> search (( == ) 'X') look_around1 |> List.length

let part2 = data |> search (( == ) 'A') look_around2 |> List.length
