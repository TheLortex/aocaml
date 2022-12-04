include Aoc.Misc.DefaultIntSolution
open Containers
module StringSet = Set.Make (String)

let day = 16

type ticket = int Iter.t
type rule = { name : string; range_1 : int * int; range_2 : int * int }

type t = {
  rules : rule Iter.t;
  my_ticket : ticket;
  nearby_tickets : ticket Iter.t;
}

let parse_ticket s =
  s |> String.split_on_char ',' |> Iter.of_list |> Iter.map int_of_string

let parse_input input =
  let input = Scanf.Scanning.from_channel input in
  let scan () =
    match
      Scanf.bscanf input "%[ a-zA-Z]: %d-%d or %d-%d\n"
        (fun name r1_b r1_e r2_b r2_e ->
          { name; range_1 = (r1_b, r1_e); range_2 = (r2_b, r2_e) })
    with
    | exception Scanf.Scan_failure _ -> None
    | rule -> Some rule
  in
  let rules = Iter.of_gen scan in
  Printf.printf "Scanned %d rules \n" (Iter.length rules);
  Scanf.bscanf input "\n" ();
  Scanf.bscanf input "your ticket:\n" ();
  let my_ticket = Scanf.bscanf input "%s\n" parse_ticket in
  Scanf.bscanf input "\n" ();
  Scanf.bscanf input "nearby tickets:\n" ();
  let scan () =
    match Scanf.bscanf input "%s\n" parse_ticket with
    | exception End_of_file -> None
    | tkt -> Some tkt
  in
  let nearby_tickets = Iter.of_gen scan in
  { rules; my_ticket; nearby_tickets }

let respects_any_rule rules value =
  let r1 = Iter.map (fun r -> r.range_1) rules in
  let r2 = Iter.map (fun r -> r.range_2) rules in
  Iter.append r1 r2 |> Iter.exists (fun (b, e) -> b <= value && value <= e)

let part_1 t =
  t.nearby_tickets
  |> Iter.map (fun ticket ->
         ticket
         |> Iter.filter (fun x -> not (respects_any_rule t.rules x))
         |> Iter.sum)
  |> Iter.sum

let ranges_are_valid ((r1b, r1e), (r2b, r2e)) t =
  let res =
    Iter.for_all (fun v -> (r1b <= v && v <= r1e) || (r2b <= v && v <= r2e)) t
  in
  res

let transpose iter =
  let res =
    Iter.flat_map Iter.zip_i iter
    |> Iter.group_by ~hash:(fun (i, _) -> i) ~eq:(fun (i, _) (j, _) -> i = j)
    |> Iter.map (fun lst -> Iter.of_list lst |> Iter.map (fun (_, v) -> v))
  in
  res

let get_valid_rule_columns rules columns =
  let res = Hashtbl.create 100 in
  let do_column i column =
    Hashtbl.iter
      (fun name ranges ->
        if ranges_are_valid ranges column then Hashtbl.add res (name, i) ())
      rules
  in
  Iter.iteri do_column columns;
  res

let part_2 t =
  let valid_tickets =
    Iter.filter
      (fun ticket ->
        ticket |> Iter.for_all (fun x -> respects_any_rule t.rules x))
      t.nearby_tickets
  in
  let rules =
    t.rules
    |> Iter.map (fun r -> (r.name, (r.range_1, r.range_2)))
    |> Hashtbl.of_iter
  in
  let rule_column_valid =
    get_valid_rule_columns rules
      (transpose (Iter.cons t.my_ticket valid_tickets))
  in
  let init_possibilities = Array.make (Hashtbl.length rules) StringSet.empty in
  Hashtbl.iter
    (fun (name, i) () ->
      Printf.printf "%s: %d\n%!" name i;
      init_possibilities.(i) <- StringSet.add name init_possibilities.(i))
    rule_column_valid;
  let rec find_assoc possibilities =
    let test_possibility i value =
      let copied = Array.copy possibilities in
      copied.(i) <- StringSet.singleton value;
      let rec update_poss = function
        | i when i = Array.length copied -> true
        | i -> (
            match StringSet.remove value copied.(i) with
            | new_set when StringSet.is_empty new_set -> false
            | new_set ->
                copied.(i) <- new_set;
                update_poss (i + 1))
      in
      if update_poss (i + 1) then Some copied else None
    in
    function
    | [] -> Some possibilities
    | i :: next ->
        possibilities.(i) |> StringSet.to_iter
        |> Iter.filter_map (test_possibility i)
        |> Iter.find (fun poss -> find_assoc poss next)
  in
  let my_ticket_values = Iter.to_array t.my_ticket in
  let assoc =
    find_assoc init_possibilities
      (List.init (Array.length my_ticket_values) (fun x -> x))
  in
  assoc |> Option.get_exn |> Iter.of_array |> Iter.map StringSet.choose
  |> Iter.zip_i
  |> Iter.filter (fun (_, s) -> String.prefix ~pre:"departure" s)
  |> Iter.map (fun (i, _) -> my_ticket_values.(i))
  |> Iter.fold ( * ) 1

let part_2_disabled _ = 0
let eval = function P1 -> part_1 | P2 -> part_2_disabled
