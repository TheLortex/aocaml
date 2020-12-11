include Aoc.Misc.DefaultIntSolution
open Containers
module IntSet = Stdlib.Set.Make (Int)

let day = 11

type status = Empty | Occupied | Floor

let status_pp fmt = function
  | Floor -> Fmt.pf fmt "."
  | Occupied -> Fmt.pf fmt "#"
  | Empty -> Fmt.pf fmt "L"

type t = status array array

let map_pp fmt =
  Fmt.pf fmt "%a\n"
    (Array.pp
       ~pp_sep:(fun fmt () -> Fmt.pf fmt "\n")
       (Array.pp ~pp_sep:(fun fmt () -> ()) status_pp))

(* INPUT *)

let parse_row str =
  str |> String.to_iter
  |> Iter.map (function
       | '.' -> Floor
       | '#' -> Occupied
       | 'L' -> Empty
       | _ -> failwith "err")
  |> Iter.to_array

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let scan () =
    match Scanf.bscanf input "%s\n" (fun x -> parse_row x) with
    | exception End_of_file -> None
    | num -> Some num
  in
  Iter.of_gen scan |> Iter.to_array

(* SOLVING *)

let directions =
  let r = Iter.int_range ~start:(-1) ~stop:1 in
  Iter.product r r |> Iter.filter (fun (di, dj) -> di <> 0 || dj <> 0)

let neighbors i j = directions |> Iter.map (fun (di, dj) -> (i + di, j + dj))

let get ?(default = Floor) t i j =
  match t.(i).(j) with exception Invalid_argument _ -> default | x -> x

let count_occupied_cells_neighbors t i j =
  let is_cell_occupied i j =
    match get t i j with Occupied -> true | _ -> false
  in
  neighbors i j |> Iter.filter_count (fun (i, j) -> is_cell_occupied i j)

let part_1_occupied_to_empty t i j = count_occupied_cells_neighbors t i j >= 4

let part_1_empty_to_occupied t i j = count_occupied_cells_neighbors t i j = 0

let count_occupied_cells_line_of_sight t i j = 
  let rec raytrace di dj i j = 
    match get ~default:Empty t (di+i) (dj+j) with 
    | Occupied -> Occupied
    | Floor -> raytrace di dj (i+di) (j+dj)
    | Empty -> Empty
  in
  directions 
  |> Iter.map (fun (di,dj) -> raytrace di dj i j)
  |> Iter.filter_count (function 
    | Occupied -> true
    | _ -> false) 

let part_2_occupied_to_empty t i j = count_occupied_cells_line_of_sight t i j >= 5

let part_2_empty_to_occupied t i j = count_occupied_cells_line_of_sight t i j = 0

let state ~occupied_to_empty ~empty_to_occupied ~change t c i j =
  match c with
  | Floor -> Floor (* the floor is made of floor *)
  | Occupied ->
      if occupied_to_empty t i j then (
        change := true;
        Empty)
      else Occupied
  | Empty ->
      if empty_to_occupied t i j then (
        change := true;
        Occupied)
      else Empty

let get_next ~occupied_to_empty ~empty_to_occupied t =
  let change = ref false in
  ( !change,
    Array.mapi
      (fun i row ->
        Array.mapi
          (fun j c ->
            state ~occupied_to_empty ~empty_to_occupied ~change t c i j)
          row)
      t )

let part_1 t =
  let rec exec t =
    match
      get_next 
        ~occupied_to_empty:part_1_occupied_to_empty
        ~empty_to_occupied:part_1_empty_to_occupied 
        t
    with
    | false, next -> next
    | true, next -> exec next
  in
  exec t
  |> Array.map (fun arr ->
         arr |> Array.to_iter
         |> Iter.filter_count (function Occupied -> true | _ -> false))
  |> Array.fold_left ( + ) 0

let part_2 t =
  let rec exec t =
    match get_next 
      ~occupied_to_empty:part_2_occupied_to_empty
      ~empty_to_occupied:part_2_empty_to_occupied 
      t 
    with
    | false, next -> next
    | true, next -> exec next
  in
  exec t
  |> Array.map (fun arr ->
         arr |> Array.to_iter
         |> Iter.filter_count (function Occupied -> true | _ -> false))
  |> Array.fold_left ( + ) 0

let eval = function P1 -> part_1 | P2 -> part_2
