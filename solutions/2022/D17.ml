open Eio
include Aoc.Misc.DefaultIntSolution

let day = 17

type action = Left | Right
type t = action list

let parse_input input =
  Buf_read.line input |> String.to_seq
  |> Seq.map (function '>' -> Right | '<' -> Left | _ -> failwith "u")
  |> List.of_seq

type rock = bool array array

let rocks =
  [
    [| [| true; true; true; true |] |];
    [|
      [| false; true; false |]; [| true; true; true |]; [| false; true; false |];
    |];
    [|
      [| false; false; true |]; [| false; false; true |]; [| true; true; true |];
    |];
    [| [| true |]; [| true |]; [| true |]; [| true |] |];
    [| [| true; true |]; [| true; true |] |];
  ]

let width = 7

let rock_is_valid map rock (y, x) =
  let exception Failed in
  try
    let rock_height = Array.length rock in
    let rock_width = Array.length rock.(0) in
    if x < 0 || x + rock_width > 7 || y < 0 then raise Failed
    else
      for i = 0 to rock_height - 1 do
        for j = 0 to rock_width - 1 do
          if
            rock.(rock_height - 1 - i).(j) = true
            && Hashtbl.mem map (y + i, x + j)
          then raise Failed
        done
      done;
    true
  with Failed -> false

module State = struct
  type t = {
    map : (int * int, unit) Hashtbl.t;
    rocks : rock array;
    mutable rock_index : int;
    mutable rock_position : int * int;
    mutable n_rocks : int;
    mutable current_height : int;
  }

  let init () =
    {
      map = Hashtbl.create 10000;
      rocks = Array.of_list rocks;
      rock_index = 0;
      rock_position = (3, 2);
      n_rocks = 0;
      current_height = 0;
    }

  let move_jet t direction =
    let next_position =
      let y, x = t.rock_position in
      match direction with Left -> (y, x - 1) | Right -> (y, x + 1)
    in
    if rock_is_valid t.map t.rocks.(t.rock_index) next_position then
      t.rock_position <- next_position
    else ()

  let current_height t = t.current_height

  let spawn_next_rock t =
    let y, x = t.rock_position in
    let rock = t.rocks.(t.rock_index) in
    (* we add the rock *)
    let rock_height = Array.length rock in
    let rock_width = Array.length rock.(0) in
    for i = 0 to rock_height - 1 do
      for j = 0 to rock_width - 1 do
        if rock.(rock_height - 1 - i).(j) = true then
          Hashtbl.add t.map (y + i, x + j) ()
      done
    done;
    let next_height = y + rock_height - 1 in
    if next_height > t.current_height then t.current_height <- next_height;
    (* we increment the rock *)
    t.rock_index <- (t.rock_index + 1) mod 5;
    t.n_rocks <- t.n_rocks + 1;
    (* we move the rock *)
    let new_rock_position =
      let y = current_height t + 4 in
      (y, 2)
    in
    t.rock_position <- new_rock_position

  let move_down t =
    let next_position =
      let y, x = t.rock_position in
      (y - 1, x)
    in
    if rock_is_valid t.map t.rocks.(t.rock_index) next_position then
      t.rock_position <- next_position
    else spawn_next_rock t

  let print_map t =
    let height = current_height t in
    for i = height downto 0 do
      let s = Bytes.of_string "|.......|" in
      for j = 0 to 6 do
        if Hashtbl.mem t.map (i, j) then Bytes.set s (j + 1) '#'
      done;
      Printf.printf "%s\n%!" (Bytes.unsafe_to_string s)
    done;
    Printf.printf "+-------+\n%!"

  let turn t action =
    move_jet t action;
    move_down t
  (* ;
     print_map t *)
end

let part_1 t =
  let st = State.init () in
  let instr = List.to_seq t |> Seq.cycle |> Seq.to_dispenser in
  while st.n_rocks < 2022 do
    State.turn st (instr () |> Option.get)
  done;
  State.current_height st + 1

let part_2 t =
  let st = State.init () in
  let count = 1000000000000 in

  (* little warmup *)
  List.iter (State.turn st) t;

  let rocks = st.n_rocks in
  let height = State.current_height st in

  for _ = 0 to 4 do
    (* repeated 5 times for rock cycle purposes. trust me *)
    List.iter (State.turn st) t
  done;

  let cur_rocks = st.n_rocks in
  let delta_rocks = cur_rocks - rocks in
  let delta_height = State.current_height st - height in

  let n_cycles = (count - st.n_rocks) / delta_rocks in
  let add_height = delta_height * n_cycles in
  let remaining_rocks = count - st.n_rocks - (n_cycles * delta_rocks) in

  let instr = List.to_seq t |> Seq.cycle |> Seq.to_dispenser in
  while st.n_rocks < cur_rocks + remaining_rocks do
    State.turn st (instr () |> Option.get)
  done;
  State.current_height st + 1 + add_height

let eval = function P1 -> part_1 | P2 -> part_2
