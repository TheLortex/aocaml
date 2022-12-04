include Aoc.Misc.DefaultIntSolution

type t = int Iter.t

let day = 1

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let scan () =
    match Scanf.bscanf input "%d\n" Fun.id with
    | value -> Some value
    | exception End_of_file -> None
  in
  Iter.of_gen scan

let part_1 input =
  let first = Iter.head_exn input in
  let rest = Iter.drop 1 input in
  let _, result =
    Iter.fold
      (fun (prev, count) cur ->
        if Int.compare cur prev > 0 then (cur, count + 1) else (cur, count))
      (first, 0) rest
  in
  result

type window = int * int * int

let sum (v1, v2, v3) = v1 + v2 + v3
let add v (_, v2, v3) = (v2, v3, v)

let part_2 input =
  let init_window =
    Iter.take 3 input |> Iter.to_list |> function
    | [ a; b; c ] -> (a, b, c)
    | _ -> failwith "not enough elements"
  in
  let _, result =
    Iter.fold
      (fun (window, count) value ->
        let next_window = add value window in
        if Int.compare (sum next_window) (sum window) > 0 then
          (next_window, count + 1)
        else (next_window, count))
      (init_window, 0) (Iter.drop 3 input)
  in
  result

let eval = function P1 -> part_1 | P2 -> part_2
