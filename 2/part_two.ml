let is_valid p1 p2 letter word =
  word.[p1 - 1] == letter <> (word.[p2 - 1] == letter)

let rec main total =
  match Scanf.scanf "%d-%d %c: %s\n" is_valid with
  | true -> main (total + 1)
  | false -> main total
  | exception End_of_file -> Printf.printf "%d\n" total

let () = main 0
