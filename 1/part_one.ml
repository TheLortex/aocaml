module TargetSet = Set.Make (Int)

let target_num = 2020

let rec aux set =
  match Scanf.scanf "%d\n" (fun x -> x) with
  | i -> (
      match TargetSet.find_opt i set with
      | Some _ -> Printf.printf "%d\n" (i * (target_num - i))
      | None -> aux (TargetSet.add (target_num - i) set))
  | exception End_of_file -> ()

let () = aux TargetSet.empty
