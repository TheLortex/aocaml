module TargetMap = Map.Make (Int)

let target_num = 2020

let update map numbers cur =
  List.fold_left
    (fun a b ->
      if b + cur >= target_num then a
      else TargetMap.add (target_num - (b + cur)) (b, cur) a)
    map numbers

let rec aux map numbers =
  match Scanf.scanf "%d\n" (fun x -> x) with
  | cur -> (
      match TargetMap.find_opt cur map with
      | Some (a, b) -> Printf.printf "%d\n" (cur * a * b)
      | None ->
          let updated_map = update map numbers cur in
          aux updated_map (cur :: numbers))
  | exception End_of_file -> ()

let () = aux TargetMap.empty []
