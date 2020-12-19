include Aoc.Misc.DefaultIntSolution
open Containers
module IntSet = Stdlib.Set.Make (Int)

let day = 10

type t = int Iter.t

(* INPUT *)

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let scan () =
    match Scanf.bscanf input "%d\n" (fun x -> x) with
    | exception End_of_file -> None
    | num -> Some num
  in
  Iter.of_gen scan

(* SOLVING *)
let preamble_size = 25

let part_1 t =
  let sorted = Iter.sort t in
  let count_1 = ref 0 in
  let count_3 = ref 1 in
  let last = ref 0 in
  Iter.iter
    (fun value ->
      (match value - !last with
      | 1 -> incr count_1
      | 3 -> incr count_3
      | _ -> ());
      last := value)
    sorted;
  !count_1 * !count_3

let memo fn =
  let storage = ref (Hashtbl.create 100) in
  let rec self key =
    let a, b = key in
    match Hashtbl.find_opt !storage (a, b) with
    | Some value -> value
    | None ->
        let res = fn self key in
        let () = Hashtbl.add !storage (a, b) res in
        res
  in
  self

let n_arrang sorted =
  memo @@ fun self (last_value, position) ->
  let n_elem = Array.length sorted in
  let value = sorted.(position) in
  if value - last_value > 3 then 0
  else if position = n_elem - 1 then 1
  else
    let n_we_take = self (value, position + 1) in
    let n_we_leave = self (last_value, position + 1) in
    n_we_take + n_we_leave

let part_2 t =
  let sorted = Iter.sort t |> Iter.to_array in
  n_arrang sorted (0, 0)

let eval = function P1 -> part_1 | P2 -> part_2
