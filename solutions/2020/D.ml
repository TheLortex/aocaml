include Aoc.Misc.DefaultIntSolution
open Containers
module IntSet = Set.Make (Int)

let day = 0

type t = int Iter.t

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let scan () =
    match Scanf.bscanf input "%d\n" (fun x -> x) with
    | exception End_of_file -> None
    | instr -> Some instr
  in
  Iter.of_gen scan

let part_1 t = 0

let part_2 t = 0

let eval = function P1 -> part_1 | P2 -> part_2
