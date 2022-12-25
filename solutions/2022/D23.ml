open Eio
include Aoc.Misc.DefaultIntSolution

let day = 23

type t = string list

let parse_input input = Buf_read.lines input |> List.of_seq

type position = int * int
type direction = N | S | E | W | Stay

let delta = function
  | N -> (0, -1)
  | S -> (0, 1)
  | E -> (1, 0)
  | W -> (-1, 0)
  | Stay -> (0, 0)

let opposite = function N -> S | S -> N | E -> W | W -> E | Stay -> Stay

module Position = struct
  type t = int * int

  let compare (a0, a1) (b0, b1) =
    match Int.compare a0 b0 with 0 -> Int.compare a1 b1 | v -> v
end

module PositionSet = Set.Make (Position)
module PositionMap = Map.Make (Position)

type elfs_decision = direction PositionMap.t

let is_alone ~elfs (x, y) =
  let exception No in
  try
    for i = -1 to 1 do
      for j = -1 to 1 do
        if i <> 0 || j <> 0 then
          if PositionSet.mem (x + i, y + j) elfs then raise No
      done
    done;
    true
  with No -> false

let is_direction_valid ~elfs (x, y) direction =
  let dx, dy = delta direction in
  let options =
    if dx = 0 then [ (-1, dy); (0, dy); (1, dy) ]
    else [ (dx, -1); (dx, 0); (dx, 1) ]
  in
  List.for_all
    (fun (dx, dy) -> not (PositionSet.mem (x + dx, y + dy) elfs))
    options

let choose_direction ~elfs (x, y) directions =
  match List.find_opt (is_direction_valid ~elfs (x, y)) directions with
  | Some d -> d
  | None -> Stay

let elfs_propose ~elfs directions =
  PositionSet.to_seq elfs
  |> Seq.map (fun pos ->
         ( pos,
           if is_alone ~elfs pos then Stay
           else choose_direction ~elfs pos directions ))
  |> PositionMap.of_seq

let map_direction ~propositions (x, y) direction =
  (* check that the direction of (x, y) is not taken *)
  if direction = Stay then Stay
  else
    let dx, dy = delta direction in
    match PositionMap.find_opt (x + (2 * dx), y + (2 * dy)) propositions with
    | Some d2 when d2 = opposite direction -> Stay
    | _ -> direction

let elfs_check ~propositions =
  PositionMap.mapi (map_direction ~propositions) propositions

let elfs_move ~propositions =
  PositionMap.to_seq propositions
  |> Seq.map (fun ((x, y), direction) ->
         let dx, dy = delta direction in
         (x + dx, y + dy))
  |> PositionSet.of_seq

let turn ~elfs directions =
  let propositions = elfs_propose ~elfs directions in
  let propositions = elfs_check ~propositions in
  elfs_move ~propositions

let rotate directions = List.tl directions @ [ List.hd directions ]
let start_directions = [ N; S; W; E ]

let rec rounds ~elfs n directions =
  if n = 0 then elfs
  else rounds ~elfs:(turn ~elfs directions) (n - 1) (rotate directions)

let rectangle ~elfs =
  PositionSet.fold
    (fun (x, y) ((minx, miny), (maxx, maxy)) ->
      ((min minx x, min miny y), (max maxx x, max maxy y)))
    elfs
    ((max_int, max_int), (min_int, min_int))

let surface ((a, b), (c, d)) = (d - b + 1) * (c - a + 1)

let part_1 t =
  let elfs =
    List.to_seq t
    |> Seq.mapi (fun a b -> (a, b))
    |> Seq.flat_map (fun (y, s) ->
           String.to_seqi s
           |> Seq.filter_map (fun (x, c) ->
                  if c = '#' then Some (x, y) else None))
    |> PositionSet.of_seq
  in
  let elfs = rounds ~elfs 10 start_directions in
  let rect = rectangle ~elfs in
  surface rect - PositionSet.cardinal elfs

let turn_stop ~elfs directions =
  let propositions = elfs_propose ~elfs directions in
  let propositions = elfs_check ~propositions in
  if PositionMap.for_all (fun _ v -> v = Stay) propositions then (elfs, false)
  else (elfs_move ~propositions, true)

let rec rounds_count ~elfs n directions =
  let elfs, continue = turn_stop ~elfs directions in
  if continue then rounds_count ~elfs (n + 1) (rotate directions) else n + 1

let part_2 t =
  let elfs =
    List.to_seq t
    |> Seq.mapi (fun a b -> (a, b))
    |> Seq.flat_map (fun (y, s) ->
           String.to_seqi s
           |> Seq.filter_map (fun (x, c) ->
                  if c = '#' then Some (x, y) else None))
    |> PositionSet.of_seq
  in
  rounds_count ~elfs 0 start_directions

let eval = function P1 -> part_1 | P2 -> part_2
