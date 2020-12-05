let rec map array =
  match Scanf.scanf "%s\n" (fun x -> x) with
  | line -> map (line :: array)
  | exception End_of_file -> List.rev array

let map = Array.of_list (map [])

type dimensions = { width : int; height : int }

let dimensions = { height = Array.length map; width = String.length map.(0) }

type position = { x : int; y : int }

type slope = { dx : int; dy : int }

let advance position slope =
  {
    x = position.x + slope.dx;
    y = (position.y + slope.dy) mod dimensions.width;
  }

let get map position = map.(position.x).[position.y]

let rec ballade ~slope position n_trees =
  if position.x >= dimensions.height then n_trees
  else
    match get map position with
    | '#' -> ballade ~slope (advance position slope) (n_trees + 1)
    | '.' -> ballade ~slope (advance position slope) n_trees
    | _ -> failwith "Unknown kind of cell."

let rec aux total slopes =
  match slopes with
  | [] -> total
  | slope :: q -> aux (ballade ~slope { x = 0; y = 0 } 0 * total) q

let res =
  [
    { dx = 1; dy = 1 };
    { dx = 1; dy = 3 };
    { dx = 1; dy = 5 };
    { dx = 1; dy = 7 };
    { dx = 2; dy = 1 };
  ]
  |> List.map (fun slope -> ballade ~slope { x = 0; y = 0 } 0)
  |> List.fold_left ( * ) 1

let () = Printf.printf "%d\n" res
