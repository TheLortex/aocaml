open Eio
include Aoc.Misc.DefaultIntSolution

let day = 10

type instruction = Noop | Addx of int
type t = instruction list

let line input =
  let line = Buf_read.line input in
  match String.split_on_char ' ' line with
  | [ "addx"; r ] -> Addx (int_of_string r)
  | [ "noop" ] -> Noop
  | _ -> failwith "input"

let parse_input input = Buf_read.seq line input |> List.of_seq

module CPU = struct
  type t = { mutable x : int; mutable pc : int }

  let init () = { x = 1; pc = 1 }

  type opcode = NO | AD0 | AD1 of int

  let to_opcode = function Noop -> [ NO ] | Addx n -> [ AD0; AD1 n ]

  let exec t = function
    | NO | AD0 -> t.pc <- t.pc + 1
    | AD1 n ->
        t.x <- t.x + n;
        t.pc <- t.pc + 1
end

let part_1 instrs =
  let v = CPU.init () in
  List.map CPU.to_opcode instrs
  |> List.flatten
  |> List.fold_left
       (fun acc i ->
         CPU.exec v i;
         if (v.pc + 20) mod 40 = 0 then (
           Printf.printf "=> %d %d\n" v.pc v.x;
           acc + (v.pc * v.x))
         else acc)
       0

module CRT = struct
  type t = {
    width : int;
    height : int;
    content : Bytes.t;
    mutable position : int;
  }

  let init ~width ~height =
    { width; height; content = Bytes.create (width * height); position = 0 }

  let print t =
    for i = 0 to t.height - 1 do
      print_endline (Bytes.sub_string t.content (i * t.width) t.width)
    done

  let cycle t sprite =
    let p = t.position mod 40 in
    let c = if sprite - 1 <= p && p <= sprite + 1 then '#' else '.' in
    Bytes.set t.content t.position c;
    t.position <- t.position + 1
end

let part_2 instrs =
  let height = 6 in
  let width = 40 in
  let v = CPU.init () in
  let c = CRT.init ~width ~height in
  List.map CPU.to_opcode instrs
  |> List.flatten
  |> List.iter (fun i ->
         CRT.cycle c v.x;
         CPU.exec v i);
  CRT.print c;
  0

let eval = function P1 -> part_1 | P2 -> part_2
