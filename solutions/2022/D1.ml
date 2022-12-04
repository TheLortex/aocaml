open Eio

type t = int list list

let day = 1

let rec elf ?(acc = []) input =
  try
    let line = Buf_read.line input in
    if line = "" then List.rev acc
    else elf ~acc:(int_of_string line :: acc) input
  with End_of_file -> List.rev acc

let parse_input input = Buf_read.seq elf input |> List.of_seq

let part_1 input =
  input |> List.map (List.fold_left ( + ) 0) |> List.fold_left max 0

let part_2 input =
  input
  |> List.map (List.fold_left ( + ) 0)
  |> List.sort (fun a b -> -Int.compare a b)
  |> function
  | a :: b :: c :: _ -> a + b + c
  | _ -> failwith "uh"
