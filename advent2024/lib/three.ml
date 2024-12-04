let re = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|}

let blob name = One.read_lines name |> String.concat ""

let data = blob "../input/a03.txt"

let next_mul data idx =
  let jdx = Str.search_forward re data idx in
  let num i = Str.matched_group i data |> int_of_string in
  (jdx, (num 1, num 2))

let muls_idx data =
  let rec lp idx =
    try
      let (idx, mul) = next_mul data idx in
      (idx, mul) :: lp(idx + 1)
    with _ ->
      []
  in
  lp 0

let muls data =
  data |> muls_idx |> List.map (fun (_, mul) -> mul)

let pairs f lst = List.map (fun (n, m) -> f n m) lst
let mmul = pairs ( * )
let sum lst = List.fold_left ( + ) 0 lst

let sample = {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|}
let samp1 = muls sample |> mmul |> sum

let sample2 = {|?select()@ )select()>,how()mul(627,742)<??|}
let samp2 = muls sample2 |> mmul |> sum

let part1 = muls data |> mmul |> sum

let indexes re data =
  let rec lp idx =
    try
      let jdx = Str.search_forward re data idx in
      jdx :: lp(jdx + 1)
    with _ ->
      []
  in
  lp 0

type mul = int * int

type el =
  | Mul of int * mul
  | Do of int
  | Dont of int

let muls2 data =
  data |> muls_idx |> List.map (fun (i, mul) -> Mul(i, mul))

let dos data =
  data |> indexes (Str.regexp "do()") |> List.map (fun i -> Do i)

let donts data =
  data |> indexes (Str.regexp "don't()") |> List.map (fun i -> Dont i)

let el_idx el =
  match el with
  | Mul (i, _) -> i
  | Do i -> i
  | Dont i -> i

let el_comp a b =
  el_idx a - el_idx b

let part2_muls data =
  let rec lp yes acc lst =
    match lst with
    | [] -> acc
    | x :: xs ->
      begin
        match x with
        | Mul (_, mul) ->
          if yes then
            lp yes (mul :: acc) xs
          else
            lp yes acc xs
        | Do _ -> lp true acc xs
        | Dont _ -> lp false acc xs
      end
  in

  [ muls2 data; dos data; donts data]
  |> List.concat
  |> List.sort el_comp
  |> lp true []

let part2 = data |> part2_muls |> mmul |> sum
