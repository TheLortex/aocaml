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

(*
   #
  ### => ##
   #     ##

  01234  (x+y = 2) /
0   #
1  ###     012
2 ##### =>0###
   ###    1###
    #     2###


cos(45) = sqrt(2)/2
sin(45) = cos(45)

x = sqrt(2) /2 (x - y)
y = sqrt(2) /2 (x + y)

(0, 2) => (-2, 2)
(2, 0) => (2, 2)
(4, 2) => (2, 6)
(2, 4) => (-2, 6)
*)

module Rect = struct
  let rot45 (x, y) = (x - y, x + y)
  let invrot45 (x, y) = ((x + y) / 2, (y - x) / 2)

  type t = { x : int; y : int; width : int; height : int }

  let of_circle (x, y) d =
    let x', y' = rot45 (x - d, y) in
    let x'', y'' = rot45 (x, y - d) in
    assert (y' = y'');
    let distance = x'' - x' in
    { x = x'; y = y'; width = distance; height = distance }

  type union = t list

  let invert ~bounds { x; y; width; height } =
    (* 0123456789
       0.........
       1.........
       2####XXX##
       3.........
       4.........
    *)
    let top = { x = -bounds; y = 0; width = 2 * bounds; height = y - 1 } in
    let left = { x = -bounds; y; width = bounds + x - 1; height } in
    let right =
      { x = x + width + 1; y; width = bounds - (x + width + 1); height }
    in
    let bot =
      {
        x = -bounds;
        y = y + height + 1;
        width = 2 * bounds;
        height = (2 * bounds) - (y + height + 1);
      }
    in
    [ top; bot; left; right ]

  let pp ~bounds rects =
    let map = Array.make_matrix ((2 * bounds) + 1) ((2 * bounds) + 1) ' ' in
    List.iter
      (fun { x; y; width; height } ->
        Printf.printf "%d %d => %d %d\n" x y width height;
        for i = x to x + width do
          for j = y to y + height do
            try map.(j).(bounds + i) <- '#' with Invalid_argument _ -> ()
          done
        done)
      rects;
    for i = 0 to 4 * bounds do
      Printf.printf "%s\n%!" (Array.to_seq map.(i) |> String.of_seq)
    done

  let intersect (x1a, y1a, x2a, y2a) (x1b, y1b, x2b, y2b) =
    let x1 = max x1a x1b in
    let y1 = max y1a y1b in
    let x2 = min x2a x2b in
    let y2 = min y2a y2b in
    if x1 <= x2 && y1 <= y2 then Some (x1, y1, x2, y2) else None

  let to_coor { x; y; width; height } = (x, y, x + width, y + height)
  let of_coor (x, y, x2, y2) = { x; y; width = x2 - x; height = y2 - y }
  let intersect a b = intersect (to_coor a) (to_coor b) |> Option.map of_coor
  let intersect_gen lb a = List.filter_map (intersect a) lb
  let insersect_lst la lb = List.map (intersect_gen la) lb |> List.flatten
  let surface { width; height; _ } = (width + 1) * (height + 1)
end

let part_2 reports =
  let range = if List.length reports = 14 then 20 else 4000000 in

  let rects =
    List.map
      (fun { sensor; beacon } ->
        Rect.of_circle sensor (manhattan sensor beacon))
      reports
  in
  let lst = List.map (Rect.invert ~bounds:range) rects in
  let instr = List.fold_left Rect.insersect_lst (List.hd lst) (List.tl lst) in
  let v = List.find (fun v -> Rect.surface v = 1) instr in
  let x, y = Rect.invrot45 (v.x, v.y) in
  (x * 4000000) + y

let eval = function P1 -> part_1 | P2 -> part_2
