module Edge = struct
  type t = | N of int * int | D of char * char

  let compare a b =
    match (a, b) with
    | (N (a1, a2), N (b1, b2)) -> a1 - a2 - b1 - b2
    | (D (a1, a2), D (b1, b2)) -> (Char.compare a1 b1) - (Char.compare a2 b2)
    | _ -> raise (Invalid_argument "one graph at a time")
end

module Edges = Set.Make(Edge)

let numeric =
  [[7; 8; 9];
   [4; 5; 6];
   [1; 2; 3];
   [-1; 0; 10]]

let a = 10

let numeric = Edges.of_list [
    N(7, 8); N(7, 4);
    N(8, 9); N(8, 5);
    N(9, 6);
    N(4, 5); N(4, 1);
    N(5, 6); N(5, 2);
    N(6, 3);
    N(1, 2);
    N(2, 3); N(2, 0);
    N(3, a)
  ]

let directional =
  [['!'; '^'; 'A'];
   ['<'; 'v'; '>']]

let directional = Edges.of_list [
    D('^', 'A'); D('^', 'v');
    D('A', '>');
    D('<', 'v'); D('v', '>');
  ]

(*
module Cmap = Map.Make(Char)

let dir_mv = Cmap.of_list [
    '!', (0, 0);
    '^', (0, -1);
    'A', (0, 0);
    '<', (-1, 0);
    'v', (0, 1);
    '>', (1, 0)
  ]

let get_dir (x, y) =
  let xs = List.nth directional y in
  let ch = List.nth xs x in
  Cmap.find ch dir_mv

let move num dir =
  let (dx, dy) = get_dir dir in
  let (x, y) = dir in
  (x + dx, y + dy)
*)
