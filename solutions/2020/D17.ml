include Aoc.Misc.DefaultIntSolution
open Containers

module IntTriple = struct
  type t = int * int * int

  let compare = Stdlib.compare
end

module IntQuad = struct
  type t = int * int * int * int

  let compare = Stdlib.compare
end

let day = 17

type cell = On | Off
type t = cell Iter.t Iter.t

let parse_cell = function '.' -> Off | '#' -> On | _ -> failwith "unk"

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let scan () =
    match
      Scanf.bscanf input "%s\n" (fun x ->
          x |> String.to_iter |> Iter.map parse_cell)
    with
    | exception End_of_file -> None
    | instr -> Some instr
  in
  Iter.of_gen scan

module Simulator (E : Set.OrderedType) = struct
  module CoorSet = Set.Make (E)

  let simulate ~get_neighbours state =
    let to_remove = ref CoorSet.empty in
    let to_check_add = ref CoorSet.empty in
    let check_removals pos =
      let count_neighbours_alive =
        pos |> get_neighbours
        |> Iter.filter_count (fun pos ->
               if CoorSet.mem pos !state then true
               else (
                 to_check_add := CoorSet.add pos !to_check_add;
                 false))
      in
      if count_neighbours_alive <> 2 && count_neighbours_alive <> 3 then
        to_remove := CoorSet.add pos !to_remove
    in
    CoorSet.iter check_removals !state;
    let to_add = ref CoorSet.empty in
    let check_additions pos =
      let count_neighbours_alive =
        pos |> get_neighbours
        |> Iter.filter_count (fun pos -> CoorSet.mem pos !state)
      in
      if count_neighbours_alive = 3 then to_add := CoorSet.add pos !to_add
    in
    CoorSet.iter check_additions !to_check_add;
    state := CoorSet.diff !state !to_remove;
    state := CoorSet.union !state !to_add
end

module Simulator3D = Simulator (IntTriple)

let get_neighbours_3D (x, y, z) =
  let n = Iter.of_list [ -1; 0; 1 ] in
  Iter.product n (Iter.product n n)
  |> Iter.filter (fun (dx, (dy, dz)) -> dx <> 0 || dy <> 0 || dz <> 0)
  |> Iter.map (fun (dx, (dy, dz)) -> (x + dx, y + dy, z + dz))

let part_1 (t : t) =
  let state = ref Simulator3D.CoorSet.empty in
  t
  |> Iter.mapi (fun x v -> Iter.mapi (fun y v -> ((x, y), v)) v)
  |> Iter.flatten
  |> Iter.iter (fun ((x, y), v) ->
         match v with
         | On -> state := Simulator3D.CoorSet.add (x, y, 0) !state
         | Off -> ());
  for i = 0 to 5 do
    Simulator3D.simulate ~get_neighbours:get_neighbours_3D state
  done;
  Simulator3D.CoorSet.cardinal !state

module Simulator4D = Simulator (IntQuad)

let get_neighbours_4D (x, y, z, w) =
  let n = Iter.of_list [ -1; 0; 1 ] in
  let n_n = Iter.product n n in
  Iter.product n_n n_n
  |> Iter.filter (fun ((dx, dy), (dz, dw)) ->
         dx <> 0 || dy <> 0 || dz <> 0 || dw <> 0)
  |> Iter.map (fun ((dx, dy), (dz, dw)) -> (x + dx, y + dy, z + dz, w + dw))

let part_2 t =
  let state = ref Simulator4D.CoorSet.empty in
  t
  |> Iter.mapi (fun x v -> Iter.mapi (fun y v -> ((x, y), v)) v)
  |> Iter.flatten
  |> Iter.iter (fun ((x, y), v) ->
         match v with
         | On -> state := Simulator4D.CoorSet.add (x, y, 0, 0) !state
         | Off -> ());
  for i = 0 to 5 do
    Simulator4D.simulate ~get_neighbours:get_neighbours_4D state
  done;
  Simulator4D.CoorSet.cardinal !state

let eval = function P1 -> part_1 | P2 -> part_2
