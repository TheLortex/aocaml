include Aoc.Misc.DefaultIntSolution
open Eio

let priority = function
  | 'a' .. 'z' as c -> Char.code c - Char.code 'a' + 1
  | 'A' .. 'Z' as c -> Char.code c - Char.code 'A' + 27
  | _ -> assert false

type t = (string * string) list

let day = 3

let line input =
  let line = Buf_read.line input in
  let len = String.length line in
  (String.sub line 0 (len / 2), String.sub line (len / 2) (len / 2))

let parse_input input = Buf_read.seq line input |> List.of_seq

let score_common (a, b) =
  let s = Array.make 53 false in
  String.iter (fun c -> s.(priority c) <- true) a;
  b |> String.to_seq
  |> Seq.find_map (fun c -> if s.(priority c) then Some (priority c) else None)
  |> Option.get

let part_1 input = List.map score_common input |> List.fold_left ( + ) 0

let score_common a b c =
  let s = Array.make 53 0 in
  String.iter (fun c -> s.(priority c) <- 1) a;
  String.iter
    (fun c -> if s.(priority c) = 1 then s.(priority c) <- 2 else ())
    b;
  c |> String.to_seq
  |> Seq.find_map (fun c ->
         if s.(priority c) = 2 then Some (priority c) else None)
  |> Option.get

let join (a, b) = a ^ b

let rec loop total = function
  | [] -> total
  | a :: b :: c :: rest ->
      loop (total + score_common (join a) (join b) (join c)) rest
  | _ -> failwith ""

let part_2 input = loop 0 input
let eval = function P1 -> part_1 | P2 -> part_2
