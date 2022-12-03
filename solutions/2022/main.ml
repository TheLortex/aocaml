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

let year = 2022

let days : (module Aoc.Solution) list =
  [ (module D1); (module D2); (module D3) ]

let rec get_last = function
  | [ x ] -> x
  | _ :: q -> get_last q
  | [] -> failwith "Empty list."

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  match (!last, !stdin) with
  | false, false -> Aoc.main ~env ~year days
  | true, false -> Aoc.main ~env ~year [ get_last days ]
  | true, true -> Aoc.main ~env ~stdin:true ~year [ get_last days ]
  | false, true -> failwith "Cannot use stdin when executing all days."
