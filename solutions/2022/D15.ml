open Eio
include Aoc.Misc.DefaultIntSolution

let day = 15

type position = int * int
type report = { sensor : position; beacon : position }
type t = report list

let parse_number b =
  let n =
    Eio.Buf_read.take_while1
      (function '0' .. '9' | '-' -> true | _ -> false)
      b
  in
  int_of_string n

let parse_report i =
  Buf_read.string "Sensor at x=" i;
  let sx = parse_number i in
  Buf_read.string ", y=" i;
  let sy = parse_number i in
  Buf_read.string ": closest beacon is at x=" i;
  let bx = parse_number i in
  Buf_read.string ", y=" i;
  let by = parse_number i in
  Buf_read.string "\n" i;
  { sensor = (sx, sy); beacon = (bx, by) }

let parse_input input = Buf_read.seq parse_report input |> List.of_seq
let manhattan (x, y) (px, py) = abs (x - px) + abs (y - py)
let in_radius s x = manhattan s.sensor x <= manhattan s.sensor s.beacon

(* (x, y) tq in_radius s (x, y)  *)
let intersection ~y s =
  let sx, sy = s.sensor in
  let distance = manhattan s.sensor s.beacon in
  let deltaline = distance - abs (y - sy) in
  if deltaline < 0 then None
  else if deltaline == 0 && s.beacon = (sx, y) then None
  else
    let start =
      let xs = sx - deltaline in
      if s.beacon = (xs, y) then xs + 1 else xs
    in
    let stop =
      let xs = sx + deltaline in
      if s.beacon = (xs, y) then xs - 1 else xs
    in
    Some (start, stop)

let union intervals =
  let rec merge = function
    | [] -> []
    | [ pos ] -> [ pos ]
    | (x1, y1) :: (x2, y2) :: rest when y1 < x2 ->
        (x1, y1) :: merge ((x2, y2) :: rest)
    | (x1, y1) :: (x2, y2) :: rest when y1 >= y2 -> merge ((x1, y1) :: rest)
    | (x1, y1) :: (x2, y2) :: rest ->
        assert (y1 >= x2 && y1 < y2);
        merge ((x1, y2) :: rest)
  in
  merge (List.sort (fun v1 v2 -> Int.compare (fst v1) (fst v2)) intervals)

let () =
  assert (union [] = []);
  assert (union [ (1, 2) ] = [ (1, 2) ]);
  assert (union [ (1, 2); (2, 3) ] = [ (1, 3) ]);
  assert (union [ (1, 3); (2, 3) ] = [ (1, 3) ]);
  assert (union [ (1, 4); (2, 3); (6, 9) ] = [ (1, 4); (6, 9) ])

let length (a, b) = b - a + 1

let part_1 reports =
  let y = if List.length reports = 14 then 10 else 2000000 in
  reports
  |> List.filter_map (intersection ~y)
  |> union |> List.map length |> List.fold_left ( + ) 0

let rec interval_inter (ts, te) = function
  | [] -> []
  | (ps, pe) :: rest when pe < ts || ps > te -> interval_inter (ts, te) rest
  | (ps, pe) :: rest when ps >= ts && pe <= te ->
      (ps, pe) :: interval_inter (ts, te) rest
  | (ps, pe) :: rest when ps < ts && pe > te ->
      (ts, te) :: interval_inter (ts, te) rest
  | (ps, pe) :: rest when ps < ts -> (ts, pe) :: interval_inter (ts, te) rest
  | (ps, pe) :: rest (* when pe > te *) ->
      (ps, te) :: interval_inter (ts, te) rest

let intersection_including_beacons ~y s =
  let sx, sy = s.sensor in
  let distance = manhattan s.sensor s.beacon in
  let deltaline = distance - abs (y - sy) in
  if deltaline < 0 then None
  else
    let start = sx - deltaline in
    let stop = sx + deltaline in
    Some (start, stop)

let part_2 reports =
  let range = if List.length reports = 14 then 20 else 4000000 in
  let x, y =
    List.init range Fun.id
    |> List.find_map (fun y ->
           match
             reports
             |> List.filter_map (intersection_including_beacons ~y)
             |> union
             |> interval_inter (0, range)
           with
           | [ (0, range) ] -> None
           | [ (a, b); (c, d) ] ->
               assert (b + 2 = c);
               Some (b + 1, y)
           | _ -> failwith "no")
    |> Option.get
  in
  (x * 4_000_000) + y

let eval = function P1 -> part_1 | P2 -> part_2
