open Eio
include Aoc.Misc.DefaultIntSolution

let day = 25

type t = int list

let to_num = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '-' -> -1
  | '=' -> -2
  | _ -> failwith ""

let of_snafu = String.fold_left (fun acc c -> (acc * 5) + to_num c) 0

let to_snafu n =
  let rec aux acc n =
    if n = 0 then acc
    else
      match n mod 5 with
      | 0 -> aux ('0' :: acc) (n / 5)
      | 1 -> aux ('1' :: acc) (n / 5)
      | 2 -> aux ('2' :: acc) (n / 5)
      | 3 -> aux ('=' :: acc) ((n / 5) + 1)
      | 4 -> aux ('-' :: acc) ((n / 5) + 1)
      | _ -> failwith ""
  in
  List.to_seq (aux [] n) |> String.of_seq

let parse_input input = Buf_read.lines input |> Seq.map of_snafu |> List.of_seq
let part_1 t = List.fold_left ( + ) 0 t
let part_2 t = 0
let eval = function P1 -> part_1 | P2 -> part_2
let format fmt v = Fmt.pf fmt "%s" (to_snafu v)