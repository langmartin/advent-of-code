let rec take_while f lst =
  match lst with
  | [] -> []
  | x :: xs -> if f x then
      x :: take_while f xs
    else
      []

let rec drop_while f lst =
  match lst with
  | [] -> []
  | x :: xs -> if f x then
      drop_while f xs
    else
      xs

module IntMap = Map.Make(Int)

let has c str = String.contains str c

let page_rule s =
  match String.split_on_char '|' s with
  | a :: b :: _ -> (int_of_string a, int_of_string b)
  | _ -> (0, 0)

let add_rule page r =
  match r with
  | Some lst -> Some (page :: lst)
  | None -> Some (page :: [])

let read_order_rules file =
  let im = IntMap.empty in
  file
  |> One.read_lines
  |> take_while (has '|')
  |> List.map page_rule
  |> List.fold_left (fun m (a, b) ->
      IntMap.update a (add_rule b) m
    )
    im

let read_updates file =
  file
  |> One.read_lines
  |> drop_while (fun l -> l == "" || String.contains l '|')
  |> List.map (fun l -> l |> String.split_on_char ',' |> List.map int_of_string)

let rec is_intersection s t =
  match s with
  | [] -> false
  | x :: xs -> List.mem x t || is_intersection xs t

let rec update_ok_lp rules pages =
  match pages with
  | [] -> true
  | p :: ps -> begin
      match IntMap.find_opt p rules with
      | Some rule -> if is_intersection rule ps then false else update_ok_lp rules ps
      | None -> update_ok_lp rules ps
    end

let update_ok rules pages =
  (* the rules map early pages to ones that follow them, so if we
     check the update backwards, we should be able to say that the
     final update should not have in it's list any of the pages that
     precede it in the update. *)
  update_ok_lp rules (List.rev pages)

let middle_page upd =
  let mid = Int.div (List.length upd) 2 in
  List.nth upd mid

let samp_file = "../input/five-sample.txt"
let data_file = "../input/five.txt"

let rules = data_file |> read_order_rules

let part1 =
  data_file
  |> read_updates
  |> List.filter (update_ok rules)
  |> List.map middle_page
  |> Three.sum

let rec any f lst =
  match lst with
  | [] -> false
  | x :: xs -> f x || any f xs

let rec dfs rules f x =
  let open IntMap in
  match find_opt x rules with
  | Some xs -> any f xs || any (dfs rules f) xs
  | None -> false

let upd_compare rules a b =
  if a == b then 0 else
    (* starting at b, does a appear in the list of pages that follow b
       or one of it's followers? *)
  if dfs rules (( == ) a) b then 1 else -1

let part2 =
  data_file
  |> read_updates
  |> List.filter (Fun.negate (update_ok rules))
  |> List.map (fun upd -> List.sort (upd_compare rules) upd)
  |> List.map middle_page
  |> Three.sum
