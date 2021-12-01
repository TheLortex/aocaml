include Aoc.Misc.DefaultIntSolution
module Hashtbl = Map.Make (String)
module StringSet = Set.Make (String)

let day = 8

type op = Nop | Acc | Jmp

type instruction = op * int

type t = instruction array

(* INPUT *)

let parse_op = function
  | "jmp" -> Jmp
  | "nop" -> Nop
  | "acc" -> Acc
  | _ -> failwith "Unknown instruction."

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let rec aux instrs =
    match Scanf.bscanf input "%s %d\n" (fun x y -> (parse_op x, y)) with
    | exception End_of_file -> List.rev instrs
    | instr -> aux (instr :: instrs)
  in
  aux [] |> Array.of_list

(* SOLVING *)

type status = Instr of instruction | Visited

type execution_result = Loop of int | Terminated of int

let rec execute t ip acc =
  if ip = Array.length t then Terminated acc
  else
    match t.(ip) with
    | Visited -> Loop acc
    | Instr (Nop, _) ->
        t.(ip) <- Visited;
        execute t (ip + 1) acc
    | Instr (Acc, v) ->
        t.(ip) <- Visited;
        execute t (ip + 1) (acc + v)
    | Instr (Jmp, v) ->
        t.(ip) <- Visited;
        execute t (ip + v) acc

let part_1 t =
  match execute (Array.map (fun x -> Instr x) t) 0 0 with
  | Loop acc -> acc
  | Terminated _ -> failwith "Program terminated."

let invert = function
  | Nop, x when x <> 0 -> Some (Jmp, x)
  | Jmp, x -> Some (Nop, x)
  | _ -> None

let part_2 t =
  let rec test ip =
    match invert t.(ip) with
    | None -> test (ip + 1)
    | Some instr -> (
        let input =
          Array.mapi (fun i x -> if i = ip then Instr instr else Instr x) t
        in
        match execute input 0 0 with
        | Loop _ -> test (ip + 1)
        | Terminated acc -> acc)
  in
  test 0

let eval = function P1 -> part_1 | P2 -> part_2
