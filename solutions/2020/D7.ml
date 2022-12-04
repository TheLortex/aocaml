include Aoc.Misc.DefaultIntSolution
module Hashtbl = Map.Make (String)
module StringSet = Set.Make (String)

let day = 7

type t = int Hashtbl.t Hashtbl.t

(* INPUT *)

let parse_input input =
  let buffer = Lexing.from_channel input in
  D7_parser.main D7_lexer.token buffer
  |> List.map (fun (a, b) -> (a, b |> List.to_seq |> Hashtbl.of_seq))
  |> List.to_seq |> Hashtbl.of_seq

(* SOLVING *)
let memo fn =
  let storage = ref Hashtbl.empty in
  fun key ->
    match Hashtbl.find_opt key !storage with
    | Some value -> value
    | None ->
        let res = fn key in
        storage := Hashtbl.add key res !storage;
        res

let rec explore t =
  memo @@ fun name ->
  match Hashtbl.find_opt name t with
  | Some rules ->
      Hashtbl.fold
        (fun name _ acc -> StringSet.union acc (explore t name))
        rules (StringSet.singleton name)
  | None -> StringSet.singleton name

let reverse t =
  let res = ref Hashtbl.empty in
  Hashtbl.iter
    (fun name rules ->
      Hashtbl.iter
        (fun target _ ->
          res :=
            Hashtbl.update target
              (function
                | None -> Some (Hashtbl.singleton name 0)
                | Some map -> Some (Hashtbl.add name 0 map))
              !res)
        rules)
    t;
  !res

let part_1 t = (explore (reverse t) "shiny-gold" |> StringSet.cardinal) - 1

let rec count_bags t =
  memo @@ fun name ->
  let rules = Hashtbl.find name t in
  1
  + Hashtbl.fold
      (fun rule req total -> (req * count_bags t rule) + total)
      rules 0

let part_2 t = count_bags t "shiny-gold" - 1
let eval = function P1 -> part_1 | P2 -> part_2
