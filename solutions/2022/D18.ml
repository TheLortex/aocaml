open Eio
include Aoc.Misc.DefaultIntSolution

let day = 18

type cube = int * int * int
type t = cube list

let parse_cube i =
  Buf_read.line i |> String.split_on_char ',' |> function
  | [ a; b; c ] -> (int_of_string a, int_of_string b, int_of_string c)
  | _ -> failwith "oh no"

let parse_input input = Buf_read.seq parse_cube input |> List.of_seq

let gen ~x ~y ~z = function
  | 0 -> (x + 1, y, z)
  | 1 -> (x - 1, y, z)
  | 2 -> (x, y + 1, z)
  | 3 -> (x, y - 1, z)
  | 4 -> (x, y, z + 1)
  | 5 -> (x, y, z - 1)
  | _ -> failwith "no"

let neighbors (x, y, z) = Seq.init 6 (gen ~x ~y ~z)

module Set = Set.Make (struct
  type t = cube

  let compare (a, b, c) (d, e, f) =
    match Int.compare a d with
    | 0 -> ( match Int.compare b e with 0 -> Int.compare c f | v -> v)
    | v -> v
end)

let part_1 t =
  let kubs = Set.of_list t in
  t
  |> List.map (fun t ->
         neighbors t |> Seq.filter (fun c -> not (Set.mem c kubs)) |> Seq.length)
  |> List.fold_left ( + ) 0

let is_in_bounds (a, b, c) =
  a >= -1 && b >= -1 && c >= -1 && a <= 20 && b <= 20 && c <= 20

let outside kubs =
  let res = ref Set.empty in
  let to_do = Queue.create () in
  Queue.add (0, 0, 0) to_do;
  while not (Queue.is_empty to_do) do
    neighbors (Queue.pop to_do)
    |> Seq.filter (fun n ->
           is_in_bounds n && (not (Set.mem n !res)) && not (Set.mem n kubs))
    |> Seq.iter (fun n ->
           res := Set.add n !res;
           Queue.add n to_do)
  done;
  !res

let part_2 t =
  let kubs = Set.of_list t in
  let outside = outside kubs in
  t
  |> List.map (fun t ->
         neighbors t
         |> Seq.filter (fun c -> not (Set.mem c kubs))
         |> Seq.filter (fun c -> Set.mem c outside)
         |> Seq.length)
  |> List.fold_left ( + ) 0

let eval = function P1 -> part_1 | P2 -> part_2
