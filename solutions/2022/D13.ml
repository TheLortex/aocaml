open Eio
include Aoc.Misc.DefaultIntSolution

let day = 13

type packet = Item of int | List of packet list
type t = (packet * packet) list

let rec to_string = function
  | List lst -> Fmt.str "[%s]" (String.concat "," (List.map to_string lst))
  | Item i -> string_of_int i

let parse_number b =
  let n =
    Eio.Buf_read.take_while1 (function '0' .. '9' -> true | _ -> false) b
  in
  int_of_string n

let rec list i =
  Buf_read.char '[' i;
  let s =
    Buf_read.seq
      ~stop:(fun p -> Buf_read.peek_char p = Some ']')
      (fun i ->
        let v = item i in
        (match Buf_read.peek_char i with
        | Some ',' -> Buf_read.char ',' i
        | _ -> ());
        v)
      i
    |> List.of_seq
  in
  Buf_read.char ']' i;
  List s

and item i =
  match Buf_read.peek_char i with
  | Some '0' .. '9' -> Item (parse_number i)
  | _ -> list i

let packet input =
  let l = list input in
  Buf_read.char '\n' input;
  l

let pair_packet input =
  let p1, p2 = (packet input, packet input) in
  (* Printf.printf "%s\n%s\n\n%!" (to_string p1) (to_string p2); *)
  (try Buf_read.char '\n' input with End_of_file -> ());
  (p1, p2)

let parse_input input = Buf_read.seq pair_packet input |> List.of_seq

let rec compare a b =
  match (a, b) with
  | List l1, List l2 -> List.compare compare l1 l2
  | Item i1, Item i2 -> i1 - i2
  | Item i1, List l2 -> compare (List [ Item i1 ]) (List l2)
  | List l1, Item i2 -> compare (List l1) (List [ Item i2 ])

let part_1 pairs =
  List.mapi
    (fun idx (p1, p2) -> if compare p1 p2 >= 0 then 1 + idx else 0)
    pairs
  |> List.fold_left ( + ) 0

let part_2 pairs =
  let d1 = List [ List [ Item 2 ] ] in
  let d2 = List [ List [ Item 6 ] ] in
  let ll =
    d1 :: d2 :: (pairs |> List.map (fun (p1, p2) -> [ p1; p2 ]) |> List.flatten)
    |> List.sort compare
    |> List.mapi (fun i v -> (i, v))
  in
  let i1 =
    List.find_map (fun (i, v) -> if v = d1 then Some (i + 1) else None) ll
  in
  let i2 =
    List.find_map (fun (i, v) -> if v = d2 then Some (i + 1) else None) ll
  in
  Option.get i1 * Option.get i2

let eval = function P1 -> part_1 | P2 -> part_2
