include Aoc.Misc.DefaultIntSolution
module StringMap = Map.Make (String)

let day = 4

type t = (string * string) list list

let parse_input input =
  let rec aux passeports curpasseport = function
    | [] -> (
        match input_line input with
        | exception End_of_file when curpasseport = [] -> passeports
        | exception End_of_file -> curpasseport :: passeports
        | line when line = "" -> aux (curpasseport :: passeports) [] []
        | line -> aux passeports curpasseport (String.split_on_char ' ' line))
    | token :: next -> (
        match String.split_on_char ':' token with
        | [ key; value ] -> aux passeports ((key, value) :: curpasseport) next
        | _ -> failwith "invalid token")
  in
  aux [] [] []

(* Checkers *)
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

let is_valid_1 passeport =
  let rec aux n = function
    | [] -> n = StringMap.cardinal target_info
    | (k, _) :: next when StringMap.mem k target_info -> aux (n + 1) next
    | _ :: next -> aux n next
  in
  aux 0 passeport

let is_valid_2 passeport =
  let rec aux n = function
    | [] -> n = StringMap.cardinal target_info
    | (k, v) :: next
      when StringMap.find_opt k target_info
           |> Option.map (fun chk -> chk v)
           |> Option.value ~default:false ->
        aux (n + 1) next
    | _ :: next -> aux n next
  in
  aux 0 passeport

let part_1 t =
  t |> List.map is_valid_1
  |> List.map (function true -> 1 | false -> 0)
  |> List.fold_left ( + ) 0

let part_2 t =
  t |> List.map is_valid_2
  |> List.map (function true -> 1 | false -> 0)
  |> List.fold_left ( + ) 0

let eval = function P1 -> part_1 | P2 -> part_2
