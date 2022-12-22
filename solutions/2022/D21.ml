open Eio
include Aoc.Misc.DefaultIntSolution

let day = 21

type op = Plus | Minus | Mul | Div
type expr = Lit of int | Math of (string * op * string)
type instr = string * expr

let op_of_string = function
  | "*" -> Mul
  | "-" -> Minus
  | "+" -> Plus
  | "/" -> Div
  | _ -> failwith "parse op"

let op_to_string = function
  | Mul -> "*"
  | Minus -> "-"
  | Plus -> "+"
  | Div -> "/"

let parse_instr i =
  let open Buf_read in
  let name = take_while (( <> ) ':') i in
  string ": " i;
  match line i |> String.split_on_char ' ' with
  | [ num ] -> (name, Lit (int_of_string num))
  | [ var1; operator; var2 ] -> (name, Math (var1, op_of_string operator, var2))
  | _ -> failwith "parse"

type t = instr list

let parse_input input = Buf_read.seq parse_instr input |> List.of_seq

let exec = function
  | Mul -> ( * )
  | Div -> ( / )
  | Minus -> ( - )
  | Plus -> ( + )

let rec eval exprs target =
  match Hashtbl.find exprs target with
  | Lit v -> v
  | Math (a, op, b) -> exec op (eval exprs a) (eval exprs b)

let part_1 t =
  let exprs = List.to_seq t |> Hashtbl.of_seq in
  eval exprs "root"

type ast = Lit of int | Var | Math of (ast * op * ast)

let rec reduce = function
  | Lit v -> Lit v
  | Var -> Var
  | Math (a, op, b) -> (
      match (reduce a, reduce b) with
      | Lit a, Lit b -> Lit (exec op a b)
      | a, b -> Math (a, op, b))

let rec to_ast exprs root =
  if root = "humn" then Var
  else
    match (Hashtbl.find exprs root : expr) with
    | Lit v -> Lit v
    | Math (a, op, b) -> Math (to_ast exprs a, op, to_ast exprs b)

let equation exprs =
  match (Hashtbl.find exprs "root" : expr) with
  | Math (a, _, b) -> (to_ast exprs a, to_ast exprs b)
  | _ -> failwith ""

let rec pp ppf = function
  | Lit v -> Fmt.int ppf v
  | Math (a, op, b) -> Fmt.pf ppf "(%a) %s (%a)" pp a (op_to_string op) pp b
  | Var -> Fmt.string ppf "x"

let inv = function Mul -> Div | Div -> Mul | Minus -> Plus | Plus -> Minus
let inv_exec op = exec (inv op)

(* we assume a contains the unknown variable *)
let rec solve a b =
  match (a, b) with
  | Var, Lit v -> v
  | Math (Lit v1, op, v2), Lit v -> solve v2 (Lit (inv_exec op v v1))
  | Math (v1, op, Lit v2), Lit v -> solve v1 (Lit (inv_exec op v v2))
  | _, _ -> failwith "I don't know how to solve this equation"

let part_2 t =
  let exprs = List.to_seq t |> Hashtbl.of_seq in
  let a, b = equation exprs in
  let a = reduce a and b = reduce b in
  solve a b

let eval = function P1 -> part_1 | P2 -> part_2
