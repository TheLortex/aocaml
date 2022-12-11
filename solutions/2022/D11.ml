open Eio
include Aoc.Misc.DefaultIntSolution

let day = 11

type operation =
  | Old
  | Lit of int
  | Add of (operation * operation)
  | Mul of (operation * operation)

let rec interp old = function
  | Old -> old
  | Lit v -> v
  | Add (a, b) -> interp old a + interp old b
  | Mul (a, b) -> interp old a * interp old b

type monkey = {
  items : int list;
  operation : operation;
  checkdiv : int;
  if_true : int;
  if_false : int;
}

type t = monkey list

let parse_number b =
  let n =
    Eio.Buf_read.take_while1 (function '0' .. '9' -> true | _ -> false) b
  in
  int_of_string n

let rec op_of_string s =
  match String.split_on_char ' ' s with
  | [ "new"; "="; n1; "+"; n2 ] -> Add (op_of_string n1, op_of_string n2)
  | [ "new"; "="; n1; "*"; n2 ] -> Mul (op_of_string n1, op_of_string n2)
  | [ "old" ] -> Old
  | [ n ] -> Lit (int_of_string n)
  | _ -> failwith "op_of_string"

let parse_monkey i =
  let open Buf_read in
  string "Monkey " i;
  parse_number i |> ignore;
  string ":\n  Starting items:" i;
  let items =
    line i |> String.split_on_char ','
    |> List.map (fun s -> String.trim s |> int_of_string)
  in
  string "  Operation: " i;
  let operation = line i |> op_of_string in
  string "  Test: divisible by " i;
  let checkdiv = line i |> int_of_string in
  string "    If true: throw to monkey " i;
  let if_true = line i |> int_of_string in
  string "    If false: throw to monkey " i;
  let if_false = line i |> int_of_string in
  (match peek_char i with
  | None -> ()
  | Some '\n' -> char '\n' i
  | _ -> failwith "u");
  { items; operation; checkdiv; if_false; if_true }

let parse_input input = Buf_read.seq parse_monkey input |> List.of_seq

module Monkey : sig
  type t

  val inspections : t -> int
  val init : monkey -> t

  val round :
    calm_down:(int -> int) -> send:(target:int -> int -> unit) -> t -> unit

  val receive : t -> int -> unit
end = struct
  type t = {
    spec : monkey;
    mutable items : int list;
    mutable inspections : int;
  }

  let inspections t = t.inspections
  let init spec = { spec; items = spec.items; inspections = 0 }

  let round ~calm_down ~send t =
    let items = t.items in
    t.items <- [];
    List.iter
      (fun item ->
        Logs.debug (fun f ->
            f "Monkey inspects an item with a worry level of %d." item);
        t.inspections <- t.inspections + 1;
        let worry = interp item t.spec.operation in
        Logs.debug (fun f -> f "Worry level is now %d." worry);
        let worry = calm_down worry in
        Logs.debug (fun f ->
            f "Monkey gets bored with item. Worry level is divided by 3 to %d."
              worry);
        let divisible = worry mod t.spec.checkdiv = 0 in
        Logs.debug (fun f ->
            f "Current worry level is%s divisible by %d."
              (if divisible then "" else " not")
              t.spec.checkdiv);
        let target = if divisible then t.spec.if_true else t.spec.if_false in
        Logs.debug (fun f ->
            f "Item with worry level %d is thrown to monkey %d." worry target);
        send ~target worry)
      items

  let receive t item = t.items <- item :: t.items
end

let part_1 monkeys =
  let monkeys = List.map Monkey.init monkeys |> Array.of_list in
  let send ~target = Monkey.receive monkeys.(target) in
  for i = 1 to 20 do
    Array.iter (Monkey.round ~calm_down:(fun w -> w / 3) ~send) monkeys
  done;
  Array.to_list monkeys
  |> List.map Monkey.inspections
  |> List.sort (fun a b -> b - a)
  |> function
  | a :: b :: _ -> a * b
  | _ -> failwith "o"

let part_2 monkeys =
  let do_mod =
    List.map (fun f -> f.checkdiv) monkeys |> List.fold_left ( * ) 1
  in
  let monkeys = List.map Monkey.init monkeys |> Array.of_list in
  let send ~target = Monkey.receive monkeys.(target) in
  for i = 1 to 10000 do
    Array.iter (Monkey.round ~calm_down:(fun w -> w mod do_mod) ~send) monkeys
  done;
  Array.to_list monkeys
  |> List.map Monkey.inspections
  |> List.sort (fun a b -> b - a)
  |> function
  | a :: b :: _ -> a * b
  | _ -> failwith "o"

let eval = function P1 -> part_1 | P2 -> part_2
