include Aoc.Misc.DefaultIntSolution
module TargetSet = Set.Make (Int)
module TargetMap = Map.Make (Int)

type t = int list

let day = 1

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let rec aux lst =
    match Scanf.bscanf input "%d\n" (fun x -> x) with
    | value -> aux (value :: lst)
    | exception End_of_file -> lst
  in
  aux []

let target_num = 2020

let part_1 input =
  let rec aux set = function
    | cur :: _ when TargetSet.mem cur set -> Some (cur * (target_num - cur))
    | cur :: q -> aux (TargetSet.add (target_num - cur) set) q
    | [] -> None
  in
  Option.get (aux TargetSet.empty input)

let part_2 input =
  let update map numbers cur =
    List.fold_left
      (fun a b ->
        if b + cur >= target_num then a
        else TargetMap.add (target_num - (b + cur)) (b, cur) a)
      map numbers
  in
  let rec aux map numbers = function
    | cur :: _ when TargetMap.mem cur map ->
        let a, b = TargetMap.find cur map in
        Some (cur * a * b)
    | cur :: q ->
        let updated_map = update map numbers cur in
        aux updated_map (cur :: numbers) q
    | [] -> None
  in
  Option.get (aux TargetMap.empty [] input)

let eval = function P1 -> part_1 | P2 -> part_2
