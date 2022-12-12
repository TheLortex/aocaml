open Eio
include Aoc.Misc.DefaultIntSolution

let day = 12

type t = {
  start : int * int;
  stop : int * int;
  map : (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array2.t;
}

let parse_input input =
  let lines = Buf_read.lines input |> List.of_seq in
  let height = List.length lines in
  let width = String.length (List.hd lines) in
  let array = Bigarray.(Array2.create int c_layout) height width in
  let start = ref None in
  let stop = ref None in
  List.iteri
    (fun x ->
      String.iteri (fun y c ->
          let c =
            match c with
            | 'S' ->
                start := Some (x, y);
                'a'
            | 'E' ->
                stop := Some (x, y);
                'z'
            | c -> c
          in
          array.{x, y} <- Char.code c))
    lines;
  { map = array; start = Option.get !start; stop = Option.get !stop }

let deltas = List.to_seq [ (0, 1); (1, 0); (-1, 0); (0, -1) ]
let neighbors (a, b) = Seq.map (fun (da, db) -> (a + da, b + db)) deltas

let shortest_path ~check_ok ~start ~stop map =
  let to_do = Queue.create () in
  let explored =
    Bigarray.(Array2.init int8_unsigned c_layout)
      (Bigarray.Array2.dim1 map) (Bigarray.Array2.dim2 map) (fun _ _ -> 0)
  in
  Queue.add (start, 0) to_do;
  let exception Found of int in
  try
    while not (Queue.is_empty to_do) do
      match Queue.pop to_do with
      | position, distance when stop position -> raise (Found distance)
      | ((px, py) as position), distance ->
          neighbors position
          |> Seq.filter (fun (x, y) ->
                 try explored.{x, y} = 0 && check_ok (x, y) (px, py)
                 with Invalid_argument _ -> false)
          |> Seq.iter (fun (px, py) ->
                 explored.{px, py} <- 1;
                 Queue.add ((px ,py), distance + 1) to_do)
    done;
    raise Not_found
  with Found i -> i

let part_1 { start; stop; map } =
  shortest_path
    ~check_ok:(fun (x, y) (px, py) -> map.{x, y} <= map.{px, py} + 1)
    ~start
    ~stop:(fun p -> p = stop)
    map

let part_2 { start; stop; map } =
  shortest_path
    ~check_ok:(fun (x, y) (px, py) -> map.{px, py} <= map.{x, y} + 1)
    ~start:stop
    ~stop:(fun (x, y) -> map.{x, y} = Char.code 'a')
    map

let eval = function P1 -> part_1 | P2 -> part_2
