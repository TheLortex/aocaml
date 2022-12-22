open Eio
include Aoc.Misc.DefaultIntSolution

let day = 20

type t = int list

let parse_input input =
  Buf_read.seq (fun i -> Buf_read.line i |> int_of_string) input |> List.of_seq

type item = { v : int; mutable prev : item; mutable next : item }

let connect a b =
  assert (a.next == a);
  assert (b.prev == b);
  a.next <- b;
  b.prev <- a

let disconnect a b =
  assert (a.next == b);
  assert (b.prev == a);
  a.next <- a;
  b.prev <- b

let rec move ~length v from =
  if v < -length || v > length then move ~length (v mod length) from
  else if v = 0 then from
  else if v > 0 then move ~length (v - 1) from.next
  else move ~length (v + 1) from.prev

let mix_round array =
  for i = 0 to Array.length array - 1 do
    let node = array.(i) in
    if node.v = 0 then ()
    else
      let prev = node.prev in
      let next = node.next in
      disconnect node next;
      disconnect prev node;
      connect prev next;
      let target = move ~length:(Array.length array - 1) (node.v - 1) next in
      let target_next = target.next in
      disconnect target target_next;
      connect target node;
      connect node target_next
  done

let create t =
  let v =
    List.map
      (fun v ->
        let rec i = { v; prev = i; next = i } in
        i)
      t
    |> Array.of_list
  in
  let l = Array.length v in
  let zero_node = ref None in
  for i = 0 to l - 1 do
    if v.(i).v = 0 then zero_node := Some v.(i);
    connect v.(i) v.((i + 1) mod l)
  done;
  (v, Option.get !zero_node)

let part_1 t =
  let array, zero = create t in
  let l = Array.length array in
  mix_round array;
  let v1 = move ~length:l 1000 zero in
  let v2 = move ~length:l 2000 zero in
  let v3 = move ~length:l 3000 zero in
  v1.v + v2.v + v3.v

let decryption_key = 811589153

let part_2 t =
  let array, zero = create (t |> List.map (( * ) decryption_key)) in
  let l = Array.length array in
  for _ = 1 to 10 do
    mix_round array
  done;
  let v1 = move ~length:l 1000 zero in
  let v2 = move ~length:l 2000 zero in
  let v3 = move ~length:l 3000 zero in
  v1.v + v2.v + v3.v

let eval = function P1 -> part_1 | P2 -> part_2
