include Aoc.Misc.DefaultIntSolution
open Eio

type action = Rock | Paper | Scissors

let value = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let outcome other mine =
  match (other, mine) with
  | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6
  | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3
  | Rock, Scissors | Paper, Rock | Scissors, Paper -> 0

type t = (action * action) list

let day = 2

let action_of_string = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> failwith "o"

let line input =
  let line = Buf_read.line input in
  match String.split_on_char ' ' line with
  | [ a; b ] -> (action_of_string a, action_of_string b)
  | _ -> failwith "uh"

let parse_input input = Buf_read.seq line input |> List.of_seq
let score (other, mine) = value mine + outcome other mine
let part_1 input = List.map score input |> List.fold_left ( + ) 0

type target = Win | Lose | Draw

let target_of_action = function Rock -> Lose | Paper -> Draw | Scissors -> Win

let choose_action other target =
  match (target, other) with
  | Draw, v -> v
  | Lose, Scissors | Win, Rock -> Paper
  | Lose, Paper | Win, Scissors -> Rock
  | Lose, Rock | Win, Paper -> Scissors

let score (other, target) =
  let mine = choose_action other (target_of_action target) in
  score (other, mine)

let part_2 input = List.map score input |> List.fold_left ( + ) 0
let eval = function P1 -> part_1 | P2 -> part_2
