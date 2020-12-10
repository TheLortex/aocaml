include Aoc.Misc.DefaultIntSolution
module TargetSet = Set.Make (Int)
module TargetMap = Map.Make (Int)

type t = int Iter.t

let day = 1

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let scan () =
    match Scanf.bscanf input "%d\n" (fun x -> x) with
    | value -> Some value
    | exception End_of_file -> None
  in
  Iter.of_gen scan

let target_num = 2020

let part_1 input =
  let target_set = ref TargetSet.empty in
  let rec test cur =
    if TargetSet.mem cur !target_set then Some (cur * (target_num - cur))
    else (
      target_set := TargetSet.add (target_num - cur) !target_set;
      None)
  in
  Iter.find test input |> Option.get

let part_2 input =
  let target_map = ref TargetMap.empty in
  let update numbers cur =
    List.iter
      (fun b ->
        if b + cur < target_num then
          target_map :=
            TargetMap.add (target_num - (b + cur)) (b, cur) !target_map)
      numbers
  in
  let numbers = ref [] in
  let test cur =
    match TargetMap.find_opt cur !target_map with
    | Some (a, b) -> Some (cur * a * b)
    | None ->
        update !numbers cur;
        numbers := cur :: !numbers;
        None
  in
  Iter.find test input |> Option.get

let eval = function P1 -> part_1 | P2 -> part_2
