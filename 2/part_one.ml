let count letter word =
  String.to_seq word
  |> Seq.fold_left (fun a b -> if b == letter then a + 1 else a) 0

let is_valid min max letter word =
  let count = count letter word in
  min <= count && count <= max

let rec main total =
  match Scanf.scanf "%d-%d %c: %s\n" is_valid with
  | true -> main (total + 1)
  | false -> main total
  | exception End_of_file -> Printf.printf "%d\n" total

let () = main 0
