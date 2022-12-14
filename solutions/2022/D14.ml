open Eio
include Aoc.Misc.DefaultIntSolution

let day = 14

type position = int * int
type path = position list
type t = path list

let parse_coor s =
  match String.split_on_char ',' s with
  | [ a; b ] -> (int_of_string a, int_of_string b)
  | _ -> failwith "bad coor"

let parse_path line =
  Astring.String.cuts ~sep:" -> " line |> List.map parse_coor

let parse_input input =
  Buf_read.lines input |> Seq.map parse_path |> List.of_seq

type cell = Air | Sand | Wall

module Map : sig
  type t

  val create : int -> int -> t

  module Op : sig
    val ( .%{} ) : t -> int * int -> cell
    val ( .%{}<- ) : t -> int * int -> cell -> unit
  end
end = struct
  let (cell_of_int [@inline always]) = function
    | 0 -> Air
    | 1 -> Wall
    | 2 -> Sand
    | _ -> failwith ""

  let (int_of_cell [@inline always]) = function
    | Air -> 0
    | Wall -> 1
    | Sand -> 2

  type t =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

  let create x y =
    Bigarray.(Array2.init int8_unsigned c_layout) x y (fun _ _ ->
        int_of_cell Air)

  module Op = struct
    let ( .%{} ) map (i, j) = Bigarray.Array2.get map i j |> cell_of_int

    let ( .%{}<- ) map (i, j) v =
      Bigarray.Array2.unsafe_set map i j (int_of_cell v)
  end
end

open Map.Op

let add_segment map (sx, sy) (ex, ey) =
  let sx, ex = (min sx ex, max sx ex) in
  let sy, ey = (min sy ey, max sy ey) in
  for x = sx to ex do
    for y = sy to ey do
      map.%{(x, y)} <- Wall
    done
  done

let rec add_path map = function
  | p1 :: p2 :: rest ->
      add_segment map p1 p2;
      add_path map (p2 :: rest)
  | _ -> ()

let bot (x, y) = (x, y + 1)
let bot_left (x, y) = (x - 1, y + 1)
let bot_right (x, y) = (x + 1, y + 1)

let rec simulate_grain map hist =
  let pos = List.hd hist in
  match (map.%{bot_left pos}, map.%{bot pos}, map.%{bot_right pos}) with
  | exception Invalid_argument msg -> None
  | _, Air, _ -> simulate_grain map (bot pos :: hist)
  | Air, _, _ -> simulate_grain map (bot_left pos :: hist)
  | _, _, Air -> simulate_grain map (bot_right pos :: hist)
  | (Sand | Wall), (Sand | Wall), (Sand | Wall) -> Some (pos, List.tl hist)

let starting_position = (500, 0)

let simulate_count map =
  let rec loop count hist =
    match simulate_grain map hist with
    | None -> count
    | Some (result, hist) ->
        map.%{result} <- Sand;
        loop (count + 1) hist
  in
  loop 0 [ starting_position ]

let part_1 paths =
  let width = List.flatten paths |> List.map fst |> List.fold_left max 0 in
  let height = List.flatten paths |> List.map snd |> List.fold_left max 0 in
  let map = Map.create (width + 1) (height + 1) in
  List.iter (add_path map) paths;
  simulate_count map

let simulate_count map =
  let rec loop count hist =
    if map.%{starting_position} = Sand then count
    else
      let result, hist = simulate_grain map hist |> Option.get in
      map.%{result} <- Sand;
      loop (count + 1) hist
  in
  loop 0 [ starting_position ]

let part_2 paths =
  let width = List.flatten paths |> List.map fst |> List.fold_left max 0 in
  let height = List.flatten paths |> List.map snd |> List.fold_left max 0 in
  let paths = [ (0, height + 2); (width * 2, height + 2) ] :: paths in
  let height = height + 2 in
  let width = width * 2 in
  let map = Map.create (width + 1) (height + 1) in
  List.iter (add_path map) paths;
  simulate_count map

let eval = function P1 -> part_1 | P2 -> part_2
