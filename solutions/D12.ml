include Aoc.Misc.DefaultIntSolution
open Containers
module IntSet = Set.Make (Int)

let day = 12

type kind = N | S | E | W | L | R | F

type action = kind * int

type t = action Iter.t

(* INPUT *)

let parse_opcode = function
  | 'N' -> N
  | 'S' -> S
  | 'E' -> E
  | 'W' -> W
  | 'L' -> L
  | 'R' -> R
  | 'F' -> F
  | _ -> failwith "unknown instruction"

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let scan () =
    match Scanf.bscanf input "%c%d\n" (fun x d -> (parse_opcode x, d)) with
    | exception End_of_file -> None
    | instr -> Some instr
  in
  Iter.of_gen scan

(* SOLVING *)
type position = {
  x : int;
  y : int;
  dx : int;
  (* WEST - EAST*)
  dy : int; (* NORTH - SOUTH *)
}

let start = { x = 0; y = 0; dx = 1; dy = 0 }

type vector = { x : int; y : int }

let rec apply_rot value = function
  | angle when angle < 0 ->
      let value = apply_rot value (angle + 90) in
      { x = value.y; y = -value.x }
  | angle when angle > 0 ->
      let value = apply_rot value (angle - 90) in
      { x = -value.y; y = value.x }
  | angle when angle = 0 -> value
  | _ -> failwith "unsupported angle"

let next (state : position) (opcode, value) =
  match opcode with
  | L ->
      let rot = apply_rot { x = state.dx; y = state.dy } (-value) in
      { state with dx = rot.x; dy = rot.y }
  | R ->
      let rot = apply_rot { x = state.dx; y = state.dy } value in
      { state with dx = rot.x; dy = rot.y }
  | F ->
      {
        state with
        x = state.x + (value * state.dx);
        y = state.y + (value * state.dy);
      }
  | N -> { state with y = state.y - value }
  | S -> { state with y = state.y + value }
  | W -> { state with x = state.x - value }
  | E -> { state with x = state.x + value }

let part_1 t =
  let final_pos = Iter.fold next start t in
  Int.abs final_pos.x + Int.abs final_pos.y

let next (state : position) (opcode, value) =
  match opcode with
  | L ->
      let rot = apply_rot { x = state.dx; y = state.dy } (-value) in
      { state with dx = rot.x; dy = rot.y }
  | R ->
      let rot = apply_rot { x = state.dx; y = state.dy } value in
      { state with dx = rot.x; dy = rot.y }
  | F ->
      {
        state with
        x = state.x + (value * state.dx);
        y = state.y + (value * state.dy);
      }
  | N -> { state with dy = state.dy - value }
  | S -> { state with dy = state.dy + value }
  | W -> { state with dx = state.dx - value }
  | E -> { state with dx = state.dx + value }

let start = { x = 0; y = 0; dx = 10; dy = -1 }

let part_2 t =
  let final_pos = Iter.fold next start t in
  Int.abs final_pos.x + Int.abs final_pos.y

let eval = function P1 -> part_1 | P2 -> part_2
