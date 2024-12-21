module Ne = struct
  type t = int * int
  let compare (a1, a2) (b1, b2) = a1 - b1 - (a2 - b2)
end

module Nedges = Set.Make(Ne)

let numeric_kbd =
  [[7; 8; 9];
   [4; 5; 6];
   [1; 2; 3];
   [-1; 0; 10]]

let a = 10

let numeric = Nedges.of_list [
    (7, 8); (7, 4);
    (8, 9); (8, 5);
    (9, 6);
    (4, 5); (4, 1);
    (5, 6); (5, 2);
    (6, 3);
    (1, 2);
    (2, 3); (2, 0);
    (3, a)
  ]

module ISet = Set.Make(Int)

let numeric_nodes = numeric_kbd |> List.concat

module IMap = Map.Make(Int)

let unvisited ns dist =
  ns
  |> ISet.filter (fun x -> IMap.find x dist > 0)
  |> ISet.to_list
  |> List.sort compare

let djk g from to =
  let ns = numeric_nodes |> ISet.of_list in
  let dist = numeric_nodes |> List.map (fun n -> (n, -1) end) |> IMap.of_list in
let lp ns dist =
  match unvisited ns dist with
  | [] -> dist
  | next :: rest ->
    rest

module Edge = struct
  type t = | N of int * int | D of char * char

  let compare a b =
    match (a, b) with
    | (N (a1, a2), N (b1, b2)) -> a1 - a2 - b1 - b2
    | (D (a1, a2), D (b1, b2)) -> (Char.compare a1 b1) - (Char.compare a2 b2)
    | _ -> raise (Invalid_argument "one graph at a time")
end

module Edges = Set.Make(Edge)


let directional =
  [['!'; '^'; 'A'];
   ['<'; 'v'; '>']]

let directional = Edges.of_list [
    D('^', 'A'); D('^', 'v');
    D('A', '>');
    D('<', 'v'); D('v', '>');
  ]



(*
   1. find the shortest numeric path
   2. shortest directional path to make that
   3. ditto
*)
