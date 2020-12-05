let rec map array =
  match Scanf.scanf "%s\n" (fun x -> x) with
  | line -> map (line :: array)
  | exception End_of_file -> List.rev array

let map = Array.of_list (map [])

type dimensions = { width : int; height : int }

let dimensions = { height = Array.length map; width = String.length map.(0) }

type position = { x : int; y : int }

let advance position =
  { x = position.x + 1; y = (position.y + 3) mod dimensions.width }

let get map position = map.(position.x).[position.y]

let rec ballade position n_trees =
  if position.x >= dimensions.height then n_trees
  else
    match get map position with
    | '#' -> ballade (advance position) (n_trees + 1)
    | '.' -> ballade (advance position) n_trees
    | _ -> failwith "Unknown kind of cell."

let () = Printf.printf "%d\n" (ballade { x = 0; y = 0 } 0)
