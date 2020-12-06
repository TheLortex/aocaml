include Aoc.Misc.DefaultIntSolution

let day = 2

type password = { min : int; max : int; letter : char; word : string }

type t = password list

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let rec aux lst =
    match
      Scanf.bscanf input "%d-%d %c: %s\n" (fun min max letter word ->
          { min; max; letter; word })
    with
    | value -> aux (value :: lst)
    | exception End_of_file -> lst
  in
  aux []

let count letter word =
  String.to_seq word
  |> Seq.fold_left (fun a b -> if b == letter then a + 1 else a) 0

let is_valid_1 pwd =
  let count = count pwd.letter pwd.word in
  pwd.min <= count && count <= pwd.max

let part_1 input =
  List.map is_valid_1 input
  |> List.map (function true -> 1 | false -> 0)
  |> List.fold_left ( + ) 0

let is_valid_2 pwd =
  pwd.word.[pwd.min - 1] == pwd.letter <> (pwd.word.[pwd.max - 1] == pwd.letter)

let part_2 input =
  List.map is_valid_2 input
  |> List.map (function true -> 1 | false -> 0)
  |> List.fold_left ( + ) 0

let eval = function P1 -> part_1 | P2 -> part_2
