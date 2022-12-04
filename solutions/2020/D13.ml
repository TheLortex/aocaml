include Aoc.Misc.DefaultIntSolution
open Containers
module IntSet = Set.Make (Int)

let day = 13

type t = { start : int; bus : int option Iter.t }

(* INPUT *)

let parse_bus s =
  s |> String.split_on_char ',' |> Iter.of_list |> Iter.map int_of_string_opt

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let start = Scanf.bscanf input "%d\n" (fun x -> x) in
  let bus = Scanf.bscanf input "%s" (fun x -> parse_bus x) in
  { start; bus }

let part_1 t =
  t.bus
  |> Iter.filter_map (fun x -> x)
  |> Iter.map (fun b -> (t.start + (b - (t.start mod b)), b))
  |> Iter.min |> Option.get_exn
  |> fun (depart, bus) -> (depart - t.start) * bus

let rec euclide a = function
  | 0 -> (a, 1, 0)
  | b ->
      let r_, u_, v_ = euclide b (a mod b) in
      (r_, v_, u_ - (a / b * v_))

(* Solves (m / b) * q === 1 modulo b *)
let q m b =
  let r, u, v = euclide (m / b) b in
  assert (r = 1);
  u

(*
   t = t0 % a
   t = t1 % b
   t = t2 % c
   ...
*)
let part_2 t =
  let bus_offset =
    Iter.filter_mapi (fun i x -> Option.map (fun b -> (b, i)) x) t.bus
  in
  let m = Iter.map (fun (b, _) -> b) bus_offset |> Iter.fold ( * ) 1 in
  bus_offset |> Iter.map (fun (b, i) -> q m b * -i * (m / b) mod m) |> Iter.sum
  |> fun x -> ((x mod m) + m) mod m

let eval = function P1 -> part_1 | P2 -> part_2
