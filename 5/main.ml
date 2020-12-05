let value_of pass =
  let rec aux value = function
    | [] -> value
    | p :: q when p = 'B' || p = 'R' -> aux ((2 * value) + 1) q
    | _ :: q -> aux (2 * value) q
  in
  pass |> String.to_seq |> List.of_seq |> aux 0

let max = List.fold_left (fun cur value -> if cur > value then cur else value) 0

let input () =
  let rec aux value =
    match read_line () with
    | exception End_of_file -> value
    | str -> aux (str :: value)
  in
  aux []

let input = input () |> List.map value_of

let max_id = input |> max

let () = Printf.printf "Part 1: %d\n" max_id

module IntSet = Set.Make (Int)

let available, taken =
  let register_pass (available, taken) id =
    let taken = IntSet.add id taken in
    let available =
      [ id - 1; id; id + 1 ]
      |> List.fold_left
           (fun av id ->
             match IntSet.mem id taken with
             | true -> IntSet.remove id av
             | false -> IntSet.add id av)
           available
    in
    (available, taken)
  in
  List.fold_left register_pass (IntSet.empty, IntSet.empty) input


let () = 
  let valid id =
    id > 0 && id < max_id && 
    IntSet.mem (id+1) taken &&
    IntSet.mem (id-1) taken 
  in
  IntSet.iter
    (function
      | i when valid i -> Printf.printf "Part 2: %d\n" i | _ -> ())
    available

