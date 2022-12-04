include Aoc.Misc.DefaultIntSolution

let day = 3

type dimensions = { width : int; height : int }
type position = { x : int; y : int }
type slope = { dx : int; dy : int }
type t = { dimensions : dimensions; map : string array }

let parse_input input =
  let rec map array =
    match input_line input with
    | line -> map (line :: array)
    | exception End_of_file -> List.rev array
  in
  let map = Array.of_list (map []) in
  let dimensions =
    { height = Array.length map; width = String.length map.(0) }
  in
  { map; dimensions }

let advance t position slope =
  {
    x = position.x + slope.dx;
    y = (position.y + slope.dy) mod t.dimensions.width;
  }

let get t position = t.map.(position.x).[position.y]

let rec ballade ~slope t position n_trees =
  if position.x >= t.dimensions.height then n_trees
  else
    match get t position with
    | '#' -> ballade ~slope t (advance t position slope) (n_trees + 1)
    | '.' -> ballade ~slope t (advance t position slope) n_trees
    | _ -> failwith "Unknown kind of cell."

let part_1 t = ballade ~slope:{ dx = 1; dy = 3 } t { x = 0; y = 0 } 0

let part_2 t =
  [
    { dx = 1; dy = 1 };
    { dx = 1; dy = 3 };
    { dx = 1; dy = 5 };
    { dx = 1; dy = 7 };
    { dx = 2; dy = 1 };
  ]
  |> List.map (fun slope -> ballade ~slope t { x = 0; y = 0 } 0)
  |> List.fold_left ( * ) 1

let eval = function P1 -> part_1 | P2 -> part_2
