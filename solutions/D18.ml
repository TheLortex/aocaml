include Aoc.Misc.DefaultIntSolution
open Containers
module IntSet = Set.Make (Int)

let day = 18

type t = string Iter.t

let parse_input input =
  let scan () =
    match input_line input with
    | line -> Some line
    | exception End_of_file -> None
  in
  Iter.of_gen scan

open Angstrom

let parens p = char '(' *> p <* char ')'

let add = char '+' *> return ( + )

let mul = char '*' *> return ( * )

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

let expr : int t =
  fix (fun expr ->
      let factor = parens expr <|> integer in
      chainl1 factor (add <|> mul))

let part_1 t =
  t
  |> Iter.map (String.filter (fun c -> Stdlib.( <> ) c ' '))
  |> Iter.map (parse_string ~consume:All expr)
  |> Iter.map Result.get_exn |> Iter.sum

let expr : int t =
  fix (fun expr ->
      let factor = parens expr <|> integer in
      let term = chainl1 factor add in
      chainl1 term mul)

let part_2 t =
  t
  |> Iter.map (String.filter (fun c -> Stdlib.( <> ) c ' '))
  |> Iter.map (parse_string ~consume:All expr)
  |> Iter.map Result.get_exn |> Iter.sum

let eval = function P1 -> part_1 | P2 -> part_2
