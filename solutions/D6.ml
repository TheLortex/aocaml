include Aoc.Misc.DefaultIntSolution
module CharSet = Set.Make (Char)

let day = 6

type group = string list

type t = group list

let parse_input input =
  let rec aux groups cur_group =
    match input_line input with
    | exception End_of_file -> cur_group :: groups
    | "" -> aux (cur_group :: groups) []
    | str -> aux groups (str :: cur_group)
  in
  aux [] []

let n_any_questions_group group =
  group |> String.concat "" |> String.to_seq |> CharSet.of_seq
  |> CharSet.cardinal

let part_1 t = t |> List.map n_any_questions_group |> List.fold_left ( + ) 0

let n_every_questions_group group =
  group |> String.concat "" |> String.to_seq |> CharSet.of_seq
  |> CharSet.filter (fun c -> List.for_all (fun s -> String.contains s c) group)
  |> CharSet.cardinal

let part_2 t = t |> List.map n_every_questions_group |> List.fold_left ( + ) 0

let eval = function P1 -> part_1 | P2 -> part_2
