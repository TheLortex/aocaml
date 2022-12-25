open Eio
include Aoc.Misc.DefaultIntSolution

let day = 24

type wind = Down | Up | Left | Right

let winds = [ Down; Up; Left; Right ]

let delta = function
  | Down -> (0, 1)
  | Up -> (0, -1)
  | Left -> (-1, 0)
  | Right -> (1, 0)

type tile = Air | Wind of wind
type t = tile array array

let tile_of_char = function
  | '.' -> Air
  | 'v' -> Wind Down
  | '^' -> Wind Up
  | '<' -> Wind Left
  | '>' -> Wind Right
  | _ -> invalid_arg "none"

let parse_line i =
  let line = Buf_read.line i in
  try
    String.sub line 1 (String.length line - 2)
    |> String.to_seq |> Seq.map tile_of_char |> Array.of_seq |> Option.some
  with Invalid_argument _ -> None

let parse_input input =
  Buf_read.seq parse_line input |> Seq.filter_map Fun.id |> Array.of_seq

module Position = struct
  type t = int * int

  let compare (a0, a1) (b0, b1) =
    match Int.compare a0 b0 with 0 -> Int.compare a1 b1 | v -> v
end

module PositionSet = Set.Make (Position)

type blizzards = (wind, PositionSet.t) Hashtbl.t

let map_to_blizzards t =
  let blizzards =
    winds
    |> List.map (fun v -> (v, PositionSet.empty))
    |> List.to_seq |> Hashtbl.of_seq
  in
  let add_pos dir pos =
    let set = Hashtbl.find blizzards dir in
    Hashtbl.replace blizzards dir (PositionSet.add pos set)
  in
  for y = 0 to Array.length t - 1 do
    for x = 0 to Array.length t.(y) - 1 do
      match t.(y).(x) with Air -> () | Wind dir -> add_pos dir (x, y)
    done
  done;
  blizzards

type position = Start | End | P of Position.t

type state = {
  blizzards : blizzards;
  position : position;
  time : int;
  width : int;
  height : int;
  lcm : int;
}

(* positive modulo operator *)
let pmod a b = ((a mod b) + b) mod b

let blizzard_contact_direction state (x, y) direction =
  let positions = Hashtbl.find state.blizzards direction in
  let dx, dy = delta direction in
  (* winds move according to the delta direction. so we need to reverse the
     operation to check if there's a wind in the given position at time t *)
  let to_check =
    ( pmod (x - (dx * state.time)) state.width,
      pmod (y - (dy * state.time)) state.height )
  in
  PositionSet.mem to_check positions

let blizzard_contact state position =
  winds |> List.exists (blizzard_contact_direction state position)

let out_of_bounds state (x, y) =
  x < 0 || y < 0 || x >= state.width || y >= state.height

let is_valid state =
  match state.position with
  | Start | End -> true
  | P position ->
      not (out_of_bounds state position || blizzard_contact state position)

let rec gcd u v = if v <> 0 then gcd v (u mod v) else abs u

let lcm m n =
  match (m, n) with 0, _ | _, 0 -> 0 | m, n -> abs (m * n) / gcd m n

let hash state =
  let ph =
    match state.position with
    | Start -> 0
    | End -> 1
    | P (x, y) -> x + (state.width * y) + 2
  in
  (state.time mod state.lcm * ((state.height * state.width) + 2)) + ph

let of_map map =
  let width = Array.length map.(0) in
  let height = Array.length map in
  let lcm = lcm width height in
  {
    blizzards = map_to_blizzards map;
    position = Start;
    time = 0;
    height;
    width;
    lcm;
  }

module IntSet = Set.Make (Int)

let neighbors state =
  let state = { state with time = state.time + 1 } in
  match state.position with
  | Start -> [ state; { state with position = P (0, 0) } ]
  | End ->
      [ state; { state with position = P (state.width - 1, state.height - 1) } ]
  | P (x, y) ->
      let extra =
        if x = 0 && y = 0 then [ { state with position = Start } ]
        else if x = state.width - 1 && y = state.height - 1 then
          [ { state with position = End } ]
        else []
      in
      (state :: extra)
      @ List.map
          (fun w ->
            let dx, dy = delta w in
            { state with position = P (x + dx, y + dy) })
          winds

let bfs ~target initial_state =
  let to_do = Queue.create () in
  let explored = ref IntSet.empty in
  Queue.add initial_state to_do;
  let exception Found of state in
  try
    while not (Queue.is_empty to_do) do
      let node = Queue.pop to_do in

      if node.position = target then raise (Found node);

      neighbors node |> List.filter is_valid
      |> List.filter (fun v -> not (IntSet.mem (hash v) !explored))
      |> List.iter (fun v ->
             Queue.add v to_do;
             explored := IntSet.add (hash v) !explored)
    done;
    raise Not_found
  with Found v -> v

let part_1 t =
  let a = bfs ~target:End (of_map t) in
  a.time

let part_2 t =
  let a = bfs ~target:End (of_map t) in
  let b = bfs ~target:Start a in
  let c = bfs ~target:End b in
  c.time

let eval = function P1 -> part_1 | P2 -> part_2
