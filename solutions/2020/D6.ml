include Aoc.Misc.DefaultIntSolution

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

let a = Char.code 'a'
let z = Char.code 'z'

let n_any_questions_group group =
  let res = Array.make Char.(z - a + 1) 0 in
  let () =
    group |> String.concat ""
    |> String.iter (fun letter ->
           res.(Char.code letter - a) <- res.(Char.code letter - a) + 1)
  in
  Array.fold_left (fun i v -> if v > 0 then i + 1 else i) 0 res

let part_1 t = t |> List.map n_any_questions_group |> List.fold_left ( + ) 0

let n_every_questions_group group =
  let res = Array.make Char.(z - a + 1) 0 in
  let n_person = List.length group in
  let () =
    group
    |> List.iter (fun str ->
           String.iter
             (fun letter ->
               res.(Char.code letter - a) <- res.(Char.code letter - a) + 1)
             str)
  in
  Array.fold_left (fun i v -> if v = n_person then i + 1 else i) 0 res

let part_2 t = t |> List.map n_every_questions_group |> List.fold_left ( + ) 0
let eval = function P1 -> part_1 | P2 -> part_2
