include Aoc.Misc.DefaultIntSolution
module StringMap = Map.Make (String)

let day = 5

type t = string list

let parse_input input =
  let rec aux value =
    match input_line input with
    | exception End_of_file -> value
    | str -> aux (str :: value)
  in
  aux []

(* *)

let value_of pass =
  let rec aux value = function
    | [] -> value
    | p :: q when p = 'B' || p = 'R' -> aux ((2 * value) + 1) q
    | _ :: q -> aux (2 * value) q
  in
  pass |> String.to_seq |> List.of_seq |> aux 0

let max = List.fold_left (fun cur value -> if cur > value then cur else value) 0

let part_1 t = t |> List.map value_of |> max

module IntSet = Set.Make (Int)

let part_2 t =
  let ids = t |> List.map value_of in
  let available, taken =
    let register_pass (available, taken) id =
      let taken = IntSet.add id taken in
      let available =
        [ id - 1; id; id + 1 ]
        |> List.fold_left
             (fun av id ->
               match IntSet.mem id taken with
               | true -> IntSet.remove id av
               | false -> IntSet.add id av)
             available
      in
      (available, taken)
    in
    List.fold_left register_pass (IntSet.empty, IntSet.empty) ids
  in
  let valid id =
    id > 0
    && id < part_1 t
    && IntSet.mem (id + 1) taken
    && IntSet.mem (id - 1) taken
  in
  IntSet.find_first valid available

let eval = function P1 -> part_1 | P2 -> part_2
