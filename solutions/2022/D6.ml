open Eio
include Aoc.Misc.DefaultIntSolution

let day = 6

type t = string

let parse_input input = Buf_read.line input

let all_different sub =
  let open Astring.String.Sub in
  let a = get sub 0 in
  let b = get sub 1 in
  let c = get sub 2 in
  let d = get sub 3 in
  a <> b && a <> c && a <> d && b <> c && b <> d && c <> d

let rec find_window ~window fn str =
  let open Astring.String in
  let exception Found of int in
  try
    for i = 0 to length str - window do
      if fn (Sub.v ~start:i ~stop:(i + window) str) then raise (Found i)
    done;
    raise Not_found
  with Found i -> i

let part_1 line =
  let open Astring in
  find_window ~window:4 all_different line + 4

let count = 14

let add state c =
  let value = Hashtbl.find state c + 1 in
  Hashtbl.replace state c value;
  match value with 0 | 1 -> 0 | 2 -> 1 | _ -> 0

let rm state c =
  let value = Hashtbl.find state c - 1 in
  Hashtbl.replace state c value;
  match value with 0 -> 0 | 1 -> -1 | _ -> 0

let part_2 line =
  let state = Hashtbl.create 26 in
  for c = Char.code 'a' to Char.code 'z' do
    Hashtbl.add state (Char.chr c) 0
  done;
  let invalid = ref 0 in
  for i = 0 to count - 1 do
    invalid := !invalid + add state line.[i]
  done;
  let i = ref count in
  while !invalid > 0 do
    invalid := !invalid + add state line.[!i];
    invalid := !invalid + rm state line.[!i - count];
    incr i
  done;
  !i

let eval = function P1 -> part_1 | P2 -> part_2
