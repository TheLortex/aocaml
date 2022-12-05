open Eio
include Aoc.Misc.DefaultStringSolution

let day = 5

module State = struct
  type tower = char list
  type t = { towers : tower array }

  let parse_line line =
    let lst = ref [] in
    for i = 0 to String.length line / 4 do
      let c =
        if line.[(i * 4) + 1] = ' ' then None else Some line.[(i * 4) + 1]
      in
      lst := c :: !lst
    done;
    List.rev !lst

  let transpose lines =
    let towers = Array.make (List.length (List.hd lines)) [] in
    List.rev lines
    |> List.iter (fun line ->
           List.iteri
             (fun i item ->
               match item with
               | None -> ()
               | Some c -> towers.(i) <- c :: towers.(i))
             line);
    towers

  let parse input =
    let rec aux acc =
      match Buf_read.line input with
      | "" -> List.rev acc
      | line when line.[1] = '1' -> aux acc
      | line -> aux (parse_line line :: acc)
    in
    let lines = aux [] in
    { towers = transpose lines }
end

module Instruction = struct
  type t = { count : int; src : int; dst : int }

  let parse_number b =
    let n =
      Eio.Buf_read.take_while1 (function '0' .. '9' -> true | _ -> false) b
    in
    int_of_string n

  let parse input =
    Buf_read.string "move " input;
    let count = parse_number input in
    Buf_read.string " from " input;
    let src = parse_number input in
    Buf_read.string " to " input;
    let dst = parse_number input in
    Buf_read.char '\n' input;
    { count; src; dst }
end

type t = State.t * Instruction.t list

let parse_input input =
  let state = State.parse input in
  let instructions = Buf_read.seq Instruction.parse input |> List.of_seq in
  (state, instructions)

let take state pos =
  match state.State.towers.(pos) with
  | a :: b ->
      state.State.towers.(pos) <- b;
      a
  | _ -> assert false

let put state pos value =
  state.State.towers.(pos) <- value :: state.State.towers.(pos)

let part_1 (state, instructions) =
  let open State in
  let state = { towers = Array.copy state.towers } in
  List.iter
    (fun { Instruction.count; src; dst } ->
      for _ = 1 to count do
        put state (dst - 1) (take state (src - 1))
      done)
    instructions;
  Array.map List.hd state.towers |> Array.to_seq |> String.of_seq

let partitioni t spl =
  List.mapi (fun i v -> (i, v)) t
  |> List.partition_map (fun (i, v) ->
         if i < spl then Either.Left v else Right v)

let take state ~count pos =
  let c, r = partitioni state.State.towers.(pos) count in
  state.State.towers.(pos) <- r;
  c

let put state pos values =
  state.State.towers.(pos) <- values @ state.State.towers.(pos)

let part_2 (state, instructions) =
  let open State in
  let state = { towers = Array.copy state.towers } in
  List.iter
    (fun { Instruction.count; src; dst } ->
      put state (dst - 1) (take state ~count (src - 1)))
    instructions;
  Array.map List.hd state.towers |> Array.to_seq |> String.of_seq

let eval = function P1 -> part_1 | P2 -> part_2
