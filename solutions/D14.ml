include Aoc.Misc.DefaultIntSolution
open Containers
module IntSet = Set.Make (Int)

let day = 14

type maskbit = X | V0 | V1

type mask = maskbit array

type addr = int

type instr = Mask of mask | Set of addr * int

type t = instr Iter.t

(* INPUT *)

let reg_mask = {|mask = \([X01]+\)|} |> Str.regexp

let parse_mask s =
  s |> String.to_iter
  |> Iter.map (function
       | 'X' -> X
       | '0' -> V0
       | '1' -> V1
       | _ -> failwith "parse_mask")
  |> Iter.to_array

let reg_mem = {|mem\[\([0-9]+\)\] = \([0-9]+\)|} |> Str.regexp

let parse_line s =
  if Str.string_match reg_mask s 0 then
    Mask (Str.matched_group 1 s |> parse_mask)
  else if Str.string_match reg_mem s 0 then
    Set
      ( Str.matched_group 1 s |> int_of_string,
        Str.matched_group 2 s |> int_of_string )
  else failwith "unknown instruction"

let parse_input input =
  let scan () =
    match input_line input |> parse_line with
    | exception End_of_file -> None
    | instr -> Some instr
  in
  Iter.of_gen scan

type binary_mask = { select : int; value : int }

let to_binary_mask mask =
  mask
  |> Array.fold
       (fun (select, value) x ->
         match x with
         | X -> (2 * select, 2 * value)
         | V0 -> ((2 * select) + 1, 2 * value)
         | V1 -> ((2 * select) + 1, (2 * value) + 1))
       (0, 0)
  |> fun (select, value) -> { select; value }

let part_1 t =
  let cur_mask = ref { select = 0; value = 0 } in
  let memory = Hashtbl.create 100 in
  t
  |> Iter.iter (function
       | Mask mask -> cur_mask := to_binary_mask mask
       | Set (addr, value) ->
           Hashtbl.replace memory addr
             (value land lnot !cur_mask.select lor !cur_mask.value));
  Hashtbl.fold (fun _ v it -> it + v) memory 0

let bitsel i = 1 lsl (35 - i) (* 0 = MSB; 35 = LSB *)

let get_bit value i = value land bitsel i <> 0

let part_2 t =
  let cur_mask = ref [||] in
  let get_addresses address =
    let rec get address i =
      if i = 36 then Iter.singleton address
      else
        match (get_bit address i, !cur_mask.(i)) with
        | _, X ->
            Iter.append
              (get address (i + 1))
              (get (address lxor bitsel i) (i + 1))
        | _, V1 -> get (address lor bitsel i) (i + 1)
        | _, V0 -> get address (i + 1)
    in
    get address 0
  in
  let memory = Hashtbl.create 100 in
  t
  |> Iter.iter (function
       | Mask mask -> cur_mask := mask
       | Set (addr, value) ->
           get_addresses addr
           |> Iter.iter (fun addr -> Hashtbl.replace memory addr value));
  Hashtbl.fold (fun _ v it -> it + v) memory 0

let eval = function P1 -> part_1 | P2 -> part_2
