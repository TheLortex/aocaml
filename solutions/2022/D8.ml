open Eio
include Aoc.Misc.DefaultIntSolution

let day = 8

type t = (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

let parse_input input =
  let lines = Buf_read.lines input |> List.of_seq in
  let l1 = List.length lines in
  let l2 = String.length (List.hd lines) in
  let map = Bigarray.(Array2.create int16_unsigned c_layout l1 l2) in
  List.iteri
    (fun x ->
      String.iteri (fun y h -> map.{x, y} <- int_of_string (String.make 1 h)))
    lines;
  map

type direction = Horizontal of bool | Vertical of bool

let directions =
  [ Horizontal true; Horizontal false; Vertical true; Vertical false ]

let map_fold map i direction fn start =
  let l1 = Bigarray.Array2.dim1 map in
  let l2 = Bigarray.Array2.dim2 map in
  let range = match direction with Horizontal _ -> l1 | Vertical _ -> l2 in
  let folder acc j =
    let x, y =
      match direction with
      | Horizontal false -> (j, i)
      | Horizontal true -> (range - 1 - j, i)
      | Vertical false -> (i, j)
      | Vertical true -> (i, range - 1 - j)
    in
    fn acc x y map.{x, y}
  in
  List.init range Fun.id |> List.fold_left folder start

let ( let<> ) a f = List.iter f a

let part_1 map =
  let l1 = Bigarray.Array2.dim1 map in
  let l2 = Bigarray.Array2.dim2 map in
  assert (l1 = l2);
  let visible =
    Bigarray.(Array2.init int8_unsigned c_layout) l1 l2 (fun _ _ -> 0)
  in
  for i = 0 to l1 - 1 do
    let<> direction = directions in
    map_fold map i direction
      (fun acc x y v ->
        if v > acc then (
          visible.{x, y} <- 1;
          v)
        else acc)
      (-1)
    |> ignore
  done;
  let count = ref 0 in
  for x = 0 to l1 - 1 do
    for y = 0 to l2 - 1 do
      if visible.{x, y} > 0 then incr count
    done
  done;
  !count

module TreeRange = struct
  type tree = { size : int; position : int }
  type t = tree list
  (* invariant: sorted from shortest to highest *)

  let d a b = abs (a.position - b.position)

  (* add tree to range and count visible trees from there *)
  let add t new_tree =
    let rec add t new_tree =
      match t with
      | [] -> failwith "edge"
      | tree :: rest when tree.size < new_tree.size -> add rest new_tree
      | tree :: rest when tree.size = new_tree.size ->
          (new_tree :: rest, d tree new_tree)
      | tree :: rest -> (new_tree :: tree :: rest, d tree new_tree)
    in
    (* we cheat by adding a massive tree on the edge to block everything out *)
    match t with
    | [] -> ([ { position = new_tree.position; size = 10 } ], 0)
    | _ -> add t new_tree
end

let part_2 map =
  let l1 = Bigarray.Array2.dim1 map in
  let l2 = Bigarray.Array2.dim2 map in
  assert (l1 = l2);
  let scenic = Bigarray.(Array2.init int c_layout) l1 l2 (fun _ _ -> 1) in
  for i = 0 to l1 - 1 do
    let<> direction = directions in
    map_fold map i direction
      (fun acc x y v ->
        let next, dist =
          TreeRange.add acc
            { TreeRange.size = v; position = (if i = x then y else x) }
        in
        scenic.{x, y} <- scenic.{x, y} * dist;
        next)
      []
    |> ignore
  done;
  let max = ref 0 in
  for x = 0 to l1 - 1 do
    for y = 0 to l2 - 1 do
      if scenic.{x, y} > !max then max := scenic.{x, y}
    done
  done;
  !max

let eval = function P1 -> part_1 | P2 -> part_2
