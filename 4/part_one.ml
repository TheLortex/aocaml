module StringSet = Set.Make (String)

let target_info =
  [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ] |> StringSet.of_list

let rec is_valid number_correct = function
  | [] -> (
      match read_line () with
      | exception End_of_file ->
          (number_correct = StringSet.cardinal target_info, true)
      | line -> (
          match String.length line with
          | 0 -> (number_correct = StringSet.cardinal target_info, false)
          | _ -> is_valid number_correct (String.split_on_char ' ' line)))
  | line :: next ->
      let key = String.split_on_char ':' line |> List.hd in
      if StringSet.mem key target_info then is_valid (number_correct + 1) next
      else is_valid number_correct next

let rec algo count =
  let valid, last = is_valid 0 [] in
  let count = if valid then count + 1 else count in
  if last then count else algo count

let () = Printf.printf "%d\n" (algo 0)
