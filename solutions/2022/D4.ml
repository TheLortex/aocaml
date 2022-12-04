open Eio

let day = 4

type range = { start : int; stop : int }
type t = (range * range) list

let parse_number b =
  let n = Buf_read.take_while1 (function '0' .. '9' -> true | _ -> false) b in
  int_of_string n

let parse_range b =
  let start = parse_number b in
  Buf_read.char '-' b;
  let stop = parse_number b in
  { start; stop }

let parse_line b =
  let fst = parse_range b in
  Buf_read.char ',' b;
  let snd = parse_range b in
  Buf_read.char '\n' b;
  (fst, snd)

let parse_input input = Buf_read.seq parse_line input |> List.of_seq

let contains (a, b) =
  (a.start <= b.start && a.stop >= b.stop)
  || (b.start <= a.start && b.stop >= a.stop)

let part_1 t =
  List.map (fun c -> if contains c then 1 else 0) t |> List.fold_left ( + ) 0

let dont_overlap (a, b) = a.stop < b.start || b.stop < a.start

let part_2 t =
  List.map (fun c -> if dont_overlap c then 0 else 1) t
  |> List.fold_left ( + ) 0
