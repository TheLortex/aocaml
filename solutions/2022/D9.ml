open Eio
include Aoc.Misc.DefaultIntSolution

let day = 9

type direction = Up | Left | Right | Down
type t = (direction * int) list

let line input =
  let line = Buf_read.line input in
  match String.split_on_char ' ' line with
  | [ c; r ] ->
      let p = int_of_string r in
      ( (match c with
        | "U" -> Up
        | "D" -> Down
        | "R" -> Right
        | "L" -> Left
        | _ -> failwith "C"),
        p )
  | _ -> failwith "input"

let parse_input input = Buf_read.seq line input |> List.of_seq

type position = int * int

module Knot : sig
  type t
  type delta_raw

  val init : t
  val delta_tail : t -> delta_raw
  val delta_direction : direction -> delta_raw
  val noop : delta_raw -> bool
  val move_head_raw : t -> delta_raw -> t
  val move_tail_raw : t -> delta_raw -> t
  val tail_position : t -> position
  val move : t -> direction -> t
end = struct
  type t = { hx : int; hy : int; tx : int; ty : int }
  type delta_raw = position

  let init = { hx = 0; hy = 0; tx = 0; ty = 0 }

  let delta_direction = function
    | Up -> (0, 1)
    | Down -> (0, -1)
    | Left -> (1, 0)
    | Right -> (-1, 0)

  let fixup delta =
    (* delta is tail - head *)
    if abs delta <= 1 then 0
    else if delta == -2 then 1
    else if delta == 2 then -1
    else failwith "delta is too high!"

  let sign n = match n with 0 -> 0 | n when n > 0 -> 1 | _ -> -1

  let delta_tail t =
    let dx = t.tx - t.hx in
    let dy = t.ty - t.hy in
    match (abs dx, abs dy) with
    | 0, 0 | 0, 1 | 1, 0 | 1, 1 -> (0, 0)
    | 2, 2 | 1, 2 | 2, 1 | 2, 0 | 0, 2 -> (-sign dx, -sign dy)
    | _ -> failwith "too high"

  let move_head_raw t (dx, dy) = { t with hx = t.hx + dx; hy = t.hy + dy }
  let move_tail_raw t (dx, dy) = { t with tx = t.tx + dx; ty = t.ty + dy }
  let move_tail t = move_tail_raw t (delta_tail t)
  let move_head t direction = move_head_raw t (delta_direction direction)
  let move t direction = move_head t direction |> move_tail
  let noop = function 0, 0 -> true | _ -> false
  let tail_position t = (t.tx, t.ty)
end

module Rope : sig
  type t

  val init : int -> t
  val move : t -> direction -> t
  val tail_position : t -> position
end = struct
  type t = Knot.t list

  let rec move_raw t delta =
    match t with
    | [] -> []
    | h :: t -> (
        let h = Knot.move_head_raw h delta in
        match Knot.delta_tail h with
        | delta when Knot.noop delta -> h :: t
        | delta -> Knot.move_tail_raw h delta :: move_raw t delta)

  let move t direction = move_raw t (Knot.delta_direction direction)

  let rec tail_position = function
    | [] -> failwith "no tail"
    | [ t ] -> Knot.tail_position t
    | _ :: r -> tail_position r

  let init n = List.init n (fun _ -> Knot.init)
end

module Positions = Set.Make (struct
  type t = int * int

  let compare (a0, a1) (b0, b1) =
    match Int.compare a0 b0 with 0 -> Int.compare a1 b1 | v -> v
end)

let part_1 instrs =
  let positions = ref Positions.empty in
  let state = ref Knot.init in
  List.iter
    (fun (dir, n) ->
      for _ = 1 to n do
        state := Knot.move !state dir;
        positions := Positions.add (Knot.tail_position !state) !positions
      done)
    instrs;
  Positions.cardinal !positions

let part_2 instrs =
  let positions = ref Positions.empty in
  let state = ref (Rope.init 9) in
  List.iter
    (fun (dir, n) ->
      for _ = 1 to n do
        state := Rope.move !state dir;
        let pos = Rope.tail_position !state in
        positions := Positions.add pos !positions
      done)
    instrs;
  Positions.cardinal !positions

let eval = function P1 -> part_1 | P2 -> part_2
