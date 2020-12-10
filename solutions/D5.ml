include Aoc.Misc.DefaultIntSolution

let day = 5

type t = string list

let parse_input input =
  let rec aux value =
    match input_line input with
    | exception End_of_file -> value
    | str -> aux (str :: value)
  in
  aux []

let value_of pass =
  let len = String.length pass in
  let rec aux value = function
    | v when v = len -> value
    | v when pass.[v] = 'B' || pass.[v] = 'R' -> aux ((2 * value) + 1) (v + 1)
    | v -> aux (2 * value) (v + 1)
  in
  aux 0 0

let max = List.fold_left (fun cur value -> if cur > value then cur else value) 0

let part_1 t = t |> List.map value_of |> max

let part_2 t =
  let ids = t |> List.map value_of |> List.sort Int.compare in
  let rec aux = function
    | x :: y :: next when y - x = 2 -> x + 1
    | _ :: next -> aux next
    | [] -> raise Not_found
  in
  aux ids

let eval = function P1 -> part_1 | P2 -> part_2
