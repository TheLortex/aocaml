let ( let* ) = Rresult.( >>= )
let ( let+ ) = Rresult.( >>| )
let base_uri = Uri.of_string "https://adventofcode.com"

let rec ensuredir path =
  let dir, f = path in
  match Filename.dirname f with
  | "." -> ()
  | parent -> (
      Logs.info (fun f -> f "%s" parent);
      ensuredir (dir, parent);
      try Eio.Path.mkdir ~perm:0o700 (dir, parent)
      with Eio.Fs.Already_exists _ -> ())

let exists path =
  try Eio.Path.with_open_in path @@ fun _ -> true
  with Eio.Fs.Not_found _ -> false

let prompt_token ~file () =
  let () =
    Fmt.pr "%s%!" ("Please input your session " ^ C.orange "cookie" ^ ": ")
  in
  let token = Scanf.scanf "%s\n" (fun x -> x) in
  ensuredir file;
  Eio.Path.save ~create:(`Exclusive 0o600) file token

let get_token ~data =
  let fname = Eio.Path.(data / "token") in
  let exists = exists fname in
  if exists then () else prompt_token ~file:fname ();
  Eio.Path.load fname

exception HTTP_Error of int

let src = Logs.Src.create "aoc"

let fetch_input ~tls ~data ~net ~year ~day ~file () =
  let uri = Uri.with_path base_uri (Fmt.str "%d/day/%d/input" year day) in
  let token = get_token ~data in
  let+ input = Fetch.get ~tls ~net ~uri ~token in
  ensuredir file;
  Eio.Path.save ~create:(`Exclusive 0o600) file input

let get_input ~env ~sw ~year ~day () =
  let data = Eio.Path.(env#cwd / "data") in
  let fname =
    Eio.Path.(data / "input" / string_of_int year / string_of_int day)
  in
  let exists = exists fname in
  let+ () =
    if exists then Ok ()
    else fetch_input ~tls:env#tls ~data ~net:env#net ~year ~day ~file:fname ()
  in
  (Eio.Path.open_in ~sw fname :> Eio.Flow.source)
