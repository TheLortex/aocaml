open Eio
include Aoc.Misc.DefaultIntSolution

let day = 7

type entry_kind = File of int | Directory of entries
and entries = (string, entry_kind) Hashtbl.t

type t = entries

type command_line =
  | Cd of string
  | Ls
  | Ls_file of (int * string)
  | Ls_dir of string

let instruction input =
  if Buf_read.at_end_of_input input then Cd ".."
  else
    match Buf_read.line input |> String.split_on_char ' ' with
    | [ "$"; "cd"; pos ] -> Cd pos
    | [ "$"; "ls" ] -> Ls
    | [ "dir"; d ] -> Ls_dir d
    | [ size; file ] -> Ls_file (int_of_string size, file)
    | _ -> failwith "parse error"

let register_folder entries folder =
  match Hashtbl.find_opt entries folder with
  | None ->
      let subentries = Hashtbl.create 1 in
      Hashtbl.add entries folder (Directory subentries);
      subentries
  | Some (Directory subentries) -> subentries
  | Some _ -> assert false

let rec parse input entries =
  let exception Exit in
  try
    while true do
      match instruction input with
      | Cd ".." -> raise_notrace Exit
      | Cd "/" -> ()
      | Cd folder -> parse input (register_folder entries folder)
      | Ls -> ()
      | Ls_file (size, name) -> Hashtbl.add entries name (File size)
      | Ls_dir name -> register_folder entries name |> ignore
    done
  with Exit -> ()

let parse_input input =
  let entries = Hashtbl.create 1 in
  parse input entries;
  entries

let rec all_sizes dir acc =
  let size, acc =
    Hashtbl.fold
      (fun n v (szs, acc) ->
        match v with
        | File size -> (szs + size, acc)
        | Directory entries ->
            let size, acc = all_sizes entries acc in
            (szs + size, acc))
      dir (0, acc)
  in
  (size, size :: acc)

let part_1 folder =
  all_sizes folder [] |> snd
  (* folders that are less than 100k *)
  |> List.filter (fun n -> n < 100_000)
  (* sum *)
  |> List.fold_left ( + ) 0

let part_2 folder =
  let total_size, sizes = all_sizes folder [] in
  (sizes
  (* compute the size that is liberated if the folder is removed *)
  |> List.map (fun sz -> 70000000 - total_size + sz)
  (* filter the ones that are big enough *)
  |> List.filter (fun n -> n >= 30000000)
  (* take the smallest one *)
  |> List.sort Int.compare
  |> List.hd)
  (* compensate the initial offset *)
  + total_size
  - 70000000

let eval = function P1 -> part_1 | P2 -> part_2
