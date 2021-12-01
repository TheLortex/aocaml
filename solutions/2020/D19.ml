include Aoc.Misc.DefaultIntSolution
open Containers
module IntSet = Set.Make (Int)

let day = 19

type rule_node =
  | Or of rule_node Iter.t Iter.t
  | Char of char
  | LoopParen of (rule_node * rule_node)
  | Nop

type t = { rules : rule_node; entries : string Iter.t }

type rule_non_resolved = NOr of int Iter.t Iter.t | NChar of char

let parse_rules rules =
  if Stdlib.( = ) rules.[0] '"' then NChar rules.[1]
  else
    String.split_on_char '|' rules
    |> List.to_iter
    |> Iter.map (fun str ->
           str |> String.trim |> String.split_on_char ' '
           |> List.map int_of_string |> List.to_iter)
    |> Iter.persistent
    |> fun res -> NOr res

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let hashtbl = Hashtbl.create 100 in
  let scan_rules () =
    match
      Scanf.bscanf input "%d: %[ |\"0-9a-zA-Z]\n" (fun target rules ->
          Hashtbl.add hashtbl target (parse_rules rules))
    with
    | exception End_of_file -> None
    | exception Scanf.Scan_failure _ -> None
    | () -> Some ()
  in
  let scan_entries () =
    match Scanf.bscanf input "%s\n" (fun x -> x) with
    | exception End_of_file -> None
    | exception Scanf.Scan_failure _ -> None
    | s -> Some s
  in
  Iter.of_gen scan_rules |> Iter.iter ignore;
  let entries = Iter.of_gen scan_entries in
  let rec build_tree node_id =
    match Hashtbl.get hashtbl node_id with
    | _ when node_id = 8 -> Nop
    | _ when node_id = 11 -> Nop
    | _ when node_id = 0 -> LoopParen (build_tree 42, build_tree 31)
    | Some (NChar c) -> Char c
    | Some (NOr it) -> Or (Iter.map (Iter.map build_tree) it)
    | None -> failwith "not found"
  in
  { rules = build_tree 0; entries }

let rec build_parser_1 r =
  let open Angstrom in
  match r with
  | Char c -> char c *> return ()
  | Or disj ->
      disj
      |> Iter.map (fun b ->
             b |> Iter.map build_parser_1 |> Iter.fold ( *> ) (return ()))
      |> Iter.to_list |> choice
  | Nop -> fail "nop"
  | LoopParen (r1, r2) ->
      build_parser_1 r1 *> build_parser_1 r1 *> build_parser_1 r2

let part_1 t =
  let parser = build_parser_1 t.rules in
  t.entries
  |> Iter.filter_count (fun s ->
         match Angstrom.parse_string ~consume:All parser s with
         | Error _ -> false
         | Ok () -> true)

let rec build_parser_2 r =
  let open Angstrom in
  match r with
  | Char c -> char c *> return ()
  | Or disj ->
      disj
      |> Iter.map (fun b ->
             b |> Iter.map build_parser_2
             |> Iter.fold (fun m j -> m *> j) (return ()))
      |> Iter.to_list |> choice
  | Nop -> fail ""
  | LoopParen (r1, r2) ->
      let loopparen p1 p2 =
        fix (fun m -> p1 *> (m *> p2) <|> p1 *> (p1 *> p2) <|> p1 *> m)
      in
      loopparen (build_parser_2 r1) (build_parser_2 r2)

let part_2 t =
  let parser = build_parser_2 t.rules in
  t.entries
  |> Iter.filter_count (fun s ->
         match Angstrom.parse_string ~consume:All parser s with
         | Error _ -> false
         | Ok () -> true)

let eval = function P1 -> part_1 | P2 -> part_2
