open Eio
include Aoc.Misc.DefaultIntSolution

let day = 22

type tile = Open | Wall
type face = tile array array
type map = face option array array

let parse_map strmap =
  Printf.printf "\n%!";
  let w = Array.map String.length strmap |> Array.fold_left max 0 in
  let h = Array.length strmap in
  let nw, nh = if w > h then (4, 3) else (3, 4) in
  let face_dimens = w / nw in
  let map = Array.make_matrix nh nw None in
  for y = 0 to nh - 1 do
    for x = 0 to nw - 1 do
      try
        if strmap.(y * face_dimens).[x * face_dimens] <> ' ' then (
          let result = Array.make_matrix face_dimens face_dimens Open in
          for i = 0 to face_dimens - 1 do
            for j = 0 to face_dimens - 1 do
              if strmap.((y * face_dimens) + i).[(x * face_dimens) + j] = '#'
              then result.(i).(j) <- Wall
            done
          done;
          Printf.printf "+ %d %d\n" y x;
          map.(y).(x) <- Some result)
      with Invalid_argument _ -> ()
    done
  done;
  Printf.printf "face: %d\n" face_dimens;
  map

type rotation = RLeft | RRight
type instruction = Forward of int | Rotate of rotation
type t = { map : map; instrs : instruction list }

let read_first_part i =
  let rec loop acc =
    match Buf_read.line i with
    | "" -> List.rev acc |> Array.of_list
    | l -> loop (l :: acc)
  in
  loop []

let parse_number b =
  let n =
    Eio.Buf_read.take_while1
      (function '0' .. '9' | '-' -> true | _ -> false)
      b
  in
  int_of_string n

let read_instructions i =
  let rec loop acc =
    match Buf_read.peek_char i with
    | None -> List.rev acc
    | Some '\n' -> List.rev acc
    | Some 'L' ->
        Buf_read.char 'L' i;
        loop (Rotate RLeft :: acc)
    | Some 'R' ->
        Buf_read.char 'R' i;
        loop (Rotate RRight :: acc)
    | Some _ ->
        let n = parse_number i in
        loop (Forward n :: acc)
  in
  loop []

let parse_input input =
  let map_lines = read_first_part input in
  let map = parse_map map_lines in
  let instrs = read_instructions input in
  { map; instrs }

type orientation = Top | Bottom | Left | Right

type face_link = {
  content : face;
  x : int;
  y : int;
  neighbors : (orientation, face_link * orientation) Hashtbl.t;
}

type position = {
  face : face_link;
  x : int;
  y : int;
  orientation : orientation;
}

let rotate (rotation : rotation) =
  if rotation = RLeft then function
    | Top -> Left | Left -> Bottom | Bottom -> Right | Right -> Top
  else function Top -> Right | Right -> Bottom | Bottom -> Left | Left -> Top

let opposite = function
  | Top -> Bottom
  | Bottom -> Top
  | Left -> Right
  | Right -> Left

let delta = function
  | Top -> (0, -1)
  | Bottom -> (0, 1)
  | Left -> (-1, 0)
  | Right -> (1, 0)

let to_int = function Right -> 0 | Bottom -> 1 | Left -> 2 | Top -> 3
let to_char = function Right -> '>' | Bottom -> 'v' | Left -> '<' | Top -> '^'

type bounds = Out of orientation | Blocked | Ok

let check_bounds face x y =
  if x < 0 then Out Left
  else if y < 0 then Out Top
  else if x >= Array.length face.content then Out Right
  else if y >= Array.length face.content then Out Bottom
  else if face.content.(y).(x) = Wall then Blocked
  else Ok

let check_valid position =
  position.face.content.(position.y).(position.x) <> Wall

let rotation source target : rotation list =
  match (source, target) with
  | Top, Top | Bottom, Bottom | Left, Left | Right, Right -> []
  | Top, Left | Left, Bottom | Bottom, Right | Right, Top -> [ RLeft ]
  | Top, Right | Right, Bottom | Bottom, Left | Left, Top -> [ RRight ]
  | _ -> [ RRight; RRight ]

let apply_rotation ~face_dimens (x, y) = function
  | RLeft -> (y, face_dimens - 1 - x)
  | RRight -> (face_dimens - 1 - y, x)

(*
     0123456
   0 #######   (0, 0) => (0, 6)    (0, 0) => (6, 0)
   1 #     #   (6, 0) => (0, 0)    (6, 0) => (6, 6)
   2 #     #   (6, 6) => (6, 0)
   3 #     #   (0, 6) => (6, 6)
   4 #     #   (3, 0) => (0, 3)
   5 #     #
   6 #######
*)

let transform ~face_dimens x y old_orientation new_orientation =
  let x, y =
    ((x + face_dimens) mod face_dimens, (y + face_dimens) mod face_dimens)
  in
  let x, y =
    List.fold_left
      (apply_rotation ~face_dimens)
      (x, y)
      (rotation old_orientation (opposite new_orientation))
  in
  (x, y, opposite new_orientation)

let move position =
  let dx, dy = delta position.orientation in
  let x, y = (position.x + dx, position.y + dy) in
  let face_dimens = Array.length position.face.content in
  match check_bounds position.face x y with
  | Ok -> { position with x; y }
  | Blocked -> position
  | Out orientation ->
      (* Printf.printf "! OUT %d\n" (to_int orientation); *)
      let neighbor, new_orientation =
        Hashtbl.find position.face.neighbors orientation
      in
      let x, y, orientation =
        transform ~face_dimens x y orientation new_orientation
      in
      let new_pos = { face = neighbor; x; y; orientation } in
      if check_valid new_pos then new_pos else position

let coor position =
  let face = position.face in
  let face_dimens = Array.length face.content in
  ((face.x * face_dimens) + position.x, (face.y * face_dimens) + position.y)

let pp_pos position =
  let x, y = coor position in
  Printf.printf "%d %d %d\n" (x + 1) (y + 1) (to_int position.orientation)

let rec forward position n =
  (* pp_pos position; *)
  if n = 0 then position else forward (move position) (n - 1)

let interp position = function
  | Rotate r ->
      (* Printf.printf "=> ROT\n"; *)
      { position with orientation = rotate r position.orientation }
  | Forward n ->
      (* Printf.printf "=> %d\n" n; *)
      forward position n

let init ~faces face =
  let w, h = (Array.length faces.(0), Array.length faces) in
  [ Top; Bottom; Left; Right ]
  |> List.iter (fun orientation ->
         let dx, dy = delta orientation in
         let rec loop (tx, ty) =
           match faces.((ty + h) mod h).((tx + w) mod w) with
           | None -> loop (tx + dx, ty + dy)
           | Some v ->
               Hashtbl.add face.neighbors orientation (v, opposite orientation)
         in
         loop (face.x + dx, face.y + dy))

let pp map history =
  let cellsz = Array.length (List.hd history).face.content in
  let h = Array.length map in
  let w = Array.length map.(0) in
  let full_map = Array.make_matrix (h * cellsz) (w * cellsz) ' ' in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      match map.(i).(j) with
      | None -> ()
      | Some cell ->
          for x = 0 to cellsz - 1 do
            for y = 0 to cellsz - 1 do
              full_map.((i * cellsz) + y).((j * cellsz) + x) <-
                (if cell.content.(y).(x) = Wall then '#' else '.')
            done
          done
    done
  done;
  List.iter
    (fun v ->
      let px, py = coor v in
      full_map.(py).(px) <- to_char v.orientation)
    history;

  for i = 0 to (h * cellsz) - 1 do
    let s = Array.to_seq full_map.(i) |> String.of_seq in
    print_endline s
  done

let part_1 t =
  Printf.printf "\n\n%!";
  let faces =
    Array.mapi
      (fun y ->
        Array.mapi (fun x face ->
            Option.map
              (fun face ->
                { content = face; y; x; neighbors = Hashtbl.create 4 })
              face))
      t.map
  in
  Array.iter (Array.iter (Option.iter (init ~faces))) faces;
  let initial_face = Array.find_map Fun.id faces.(0) |> Option.get in
  let position = { face = initial_face; x = 0; y = 0; orientation = Right } in
  let position = List.fold_left interp position t.instrs in
  (* pp faces (List.rev positions); *)
  let face = position.face in
  let face_dimens = Array.length face.content in

  let mx = (face.x * face_dimens) + position.x + 1 in
  let my = (face.y * face_dimens) + position.y + 1 in
  (1000 * my) + (4 * mx) + to_int position.orientation

let neighbors = Hashtbl.create 10

(*
   ex:

     #
   ###
     ##

*)

let example () =
  let add a b =
    Hashtbl.add neighbors a b;
    Hashtbl.add neighbors b a
  in
  add ((2, 0), Right) ((3, 2), Right);
  add ((2, 0), Left) ((1, 1), Top);
  add ((2, 0), Top) ((0, 1), Top);
  add ((0, 1), Left) ((3, 2), Bottom);
  add ((0, 1), Bottom) ((2, 2), Bottom);
  add ((1, 1), Bottom) ((2, 2), Left);
  add ((2, 1), Right) ((3, 2), Top)

(*
    input:

     ##
     #
    ##
    #
*)

let real_input () =
  let add a b =
    Hashtbl.add neighbors a b;
    Hashtbl.add neighbors b a
  in
  add ((1, 0), Top) ((0, 3), Left);
  add ((1, 0), Left) ((0, 2), Left);
  add ((2, 0), Top) ((0, 3), Bottom);
  add ((2, 0), Right) ((1, 2), Right);
  add ((2, 0), Bottom) ((1, 1), Right);
  add ((1, 1), Left) ((0, 2), Top);
  add ((1, 2), Bottom) ((0, 3), Right)

let init_2 ~faces (face : face_link) =
  let w, h = (Array.length faces.(0), Array.length faces) in
  [ Top; Bottom; Left; Right ]
  |> List.iter (fun orientation ->
         match Hashtbl.find_opt neighbors ((face.x, face.y), orientation) with
         | Some ((x, y), neigh_orientation) ->
             Hashtbl.add face.neighbors orientation
               (faces.(y).(x) |> Option.get, neigh_orientation)
         | None ->
             let dx, dy = delta orientation in
             let rec loop (tx, ty) =
               match faces.((ty + h) mod h).((tx + w) mod w) with
               | None -> loop (tx + dx, ty + dy)
               | Some v ->
                   Hashtbl.add face.neighbors orientation
                     (v, opposite orientation)
             in
             loop (face.x + dx, face.y + dy))

let part_2 t =
  Hashtbl.clear neighbors;
  if Array.length t.map = 3 then example () else real_input ();
  let faces =
    Array.mapi
      (fun y ->
        Array.mapi (fun x face ->
            Option.map
              (fun face ->
                { content = face; y; x; neighbors = Hashtbl.create 4 })
              face))
      t.map
  in
  Array.iter (Array.iter (Option.iter (init_2 ~faces))) faces;

  let initial_face = Array.find_map Fun.id faces.(0) |> Option.get in

  let position = { face = initial_face; x = 0; y = 0; orientation = Right } in

  let position = List.fold_left interp position t.instrs in
  let face = position.face in
  let face_dimens = Array.length face.content in

  let mx = (face.x * face_dimens) + position.x + 1 in
  let my = (face.y * face_dimens) + position.y + 1 in
  (1000 * my) + (4 * mx) + to_int position.orientation

let eval = function P1 -> part_1 | P2 -> part_2
