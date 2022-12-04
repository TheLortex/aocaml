include Aoc.Misc.DefaultIntSolution
open Containers
module IntSet = Set.Make (Int)

let day = 15

type t = int Iter.t

let parse_input input =
  input |> input_line |> String.split_on_char ',' |> List.to_iter
  |> Iter.map int_of_string

let doit ~n t =
  let state = Hashtbl.create n in
  Iter.iteri (fun i v -> Hashtbl.add state v i) t;
  let to_speak = ref 0 in
  for i = Iter.length t to n - 2 do
    let last_seen = Hashtbl.get state !to_speak in
    Hashtbl.add state !to_speak i;
    (* say the number *)
    match last_seen with
    | None -> to_speak := 0 (* first time ? *)
    | Some date -> to_speak := i - date
  done;
  !to_speak

let part_1 = doit ~n:2020

(* Original value: 30000000 *)
let part_2 = doit ~n:3000
let eval = function P1 -> part_1 | P2 -> part_2
