let usage = "AOC: Advent of Code helper"

let last = ref false

let stdin = ref false

let args =
  Arg.parse
    [
      ("--last", Set last, "only run last day");
      ("--stdin", Set stdin, "read stdin instead of fetching input");
    ]
    (fun _ -> ())
    usage

let year = 2021

let days : (module Aoc.Solution) list = [ (module D1) ]

let rec get_last = function
  | [ x ] -> x
  | _ :: q -> get_last q
  | [] -> failwith "Empty list."

let () =
  match (!last, !stdin) with
  | false, false -> Aoc.main ~year days
  | true, false -> Aoc.main ~year [ get_last days ]
  | true, true -> Aoc.main ~stdin:true ~year [ get_last days ]
  | false, true -> failwith "Cannot use stdin when executing all days."
