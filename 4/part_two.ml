module StringMap = Map.Make (String)

type checker = string -> bool
let byr s =
  match int_of_string s with
  | exception Failure _ -> false
  | byr -> byr >= 1920 && byr <= 2002 && String.length s = 4

let iyr s =
  match int_of_string s with
  | exception Failure _ -> false
  | byr -> byr >= 2010 && byr <= 2020 && String.length s = 4

let eyr s =
  match int_of_string s with
  | exception Failure _ -> false
  | byr -> byr >= 2020 && byr <= 2030 && String.length s = 4

let hgt s =
  let length = String.length s in
  let height = String.sub s 0 (length - 2) in
  match int_of_string height with
  | exception Failure _ -> false
  | height -> (
      let unit = String.sub s (length - 2) 2 in
      match unit with
      | "cm" -> height >= 150 && height <= 193
      | "in" -> height >= 59 && height <= 76
      | _ -> false)

let hcl s =
  let reg = Str.regexp "^#[0-9a-f]*" in
  Str.string_match reg s 0

let ecl s = List.mem s [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]

let pid s =
  match int_of_string s with
  | exception Failure _ -> false
  | _ -> String.length s = 9

let target_info : checker StringMap.t =
  [
    ("byr", byr);
    ("iyr", iyr);
    ("eyr", eyr);
    ("hgt", hgt);
    ("hcl", hcl);
    ("ecl", ecl);
    ("pid", pid);
  ]
  |> List.to_seq |> StringMap.of_seq

let rec is_valid number_correct = function
  | [] -> (
      match read_line () with
      | exception End_of_file ->
          (number_correct = StringMap.cardinal target_info, true)
      | line -> (
          match String.length line with
          | 0 -> (number_correct = StringMap.cardinal target_info, false)
          | _ -> is_valid number_correct (String.split_on_char ' ' line)))
  | line :: next -> (
      match String.split_on_char ':' line with
      | [ key; value ] -> (
          match StringMap.find_opt key target_info with
          | Some checker when checker value ->
              is_valid (number_correct + 1) next
          | _ -> is_valid number_correct next)
      | _ -> is_valid number_correct next)

let rec algo count =
  let valid, last = is_valid 0 [] in
  let count = if valid then count + 1 else count in
  if last then count else algo count

let () = Printf.printf "%d\n" (algo 0)
