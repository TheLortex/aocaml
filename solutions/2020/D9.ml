include Aoc.Misc.DefaultIntSolution
open Containers
module IntSet = Stdlib.Set.Make (Int)

let day = 9

type t = int Seq.t

(* INPUT *)

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let scan () =
    match Scanf.bscanf input "%d\n" (fun x -> x) with
    | exception End_of_file -> None
    | num -> Some num
  in
  Seq.of_gen scan

(* SOLVING *)
let preamble_size = 25

let has_sum t i =
  let tranche = Array.sub t (i - preamble_size) preamble_size in
  Array.exists
    (fun v1 -> Array.exists (fun v2 -> v1 + v2 = t.(i) && v1 <> v2) tranche)
    tranche

exception Found of int

let part_1 t =
  let initial_values = t |> Seq.take preamble_size |> IntSet.of_seq in
  let values = ref initial_values in
  let t = Seq.to_array t in
  match
    for e = preamble_size to Array.length t - 1 do
      let target = t.(e) in
      let b = e - preamble_size in
      if Iter.(b -- e |> exists (fun i -> IntSet.mem (target - t.(i)) !values))
      then (
        values := IntSet.remove t.(b) !values;
        values := IntSet.add t.(e) !values)
      else raise (Found t.(e))
    done
  with
  | exception Found e -> e
  | _ -> failwith "not found"

let max = Array.fold_left max min_int

let min = Array.fold_left min max_int

let part_2 t =
  let target = part_1 t in
  let t = Seq.to_array t in
  let rec aux b e = function
    | cur when cur = target ->
        let range = Array.sub t b (e - b + 1) in
        (range |> max) + (range |> min)
    | cur when cur < target -> aux b (e + 1) (cur + t.(e + 1))
    | cur when cur > target -> aux (b + 1) e (cur - t.(b))
    | _ -> failwith "unk"
  in
  aux 0 0 t.(0)

let eval = function P1 -> part_1 | P2 -> part_2
