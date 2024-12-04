let re = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|}

let blob name = One.read_lines name |> String.concat ""

let data = blob "../input/a03.txt"

let next_mul data idx =
  let jdx = Str.search_forward re data idx in
  let num i = Str.matched_group i data |> int_of_string in
  (jdx, (num 1, num 2))

let muls data =
  let rec lp idx =
    try
      let (idx, mul) = next_mul data idx in
      mul :: lp(idx + 1)
    with _ ->
      []
  in
  lp 0

let pairs f lst = List.map (fun (n, m) -> f n m) lst
let mmul = pairs ( * )
let sum lst = List.fold_left ( + ) 0 lst

let sample = {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|}
let samp1 = muls sample |> mmul |> sum

let sample2 = {|?select()@ )select()>,how()mul(627,742)<??|}
let samp2 = muls sample2 |> mmul |> sum

let part1 = muls data |> mmul |> sum
