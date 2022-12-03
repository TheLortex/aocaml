let ( let* ) = Rresult.( >>= )
let ( let+ ) = Rresult.( >>| )
let root = "./data/"
let base_url = "https://adventofcode.com"

let ensuredir path =
  let parent, _ = Fpath.split_base path in
  Bos.OS.Dir.create ~path:true parent

let prompt_token ~file () =
  let () =
    Fmt.pr "%s%!" ("Please input your session " ^ C.orange "cookie" ^ ": ")
  in
  let token = Scanf.scanf "%s\n" (fun x -> x) in
  let* _ = ensuredir file in
  Bos.OS.File.write file token

let get_token () =
  let fname = Fpath.(v root / "token") in
  let* exists = Bos.OS.Path.exists fname in
  let* () = if exists then Ok () else prompt_token ~file:fname () in
  Bos.OS.File.read fname

exception HTTP_Error of int

let src = Logs.Src.create "aoc"

let api_request ~url ~token () =
  Logs.info ~src (fun fmt -> fmt "Performing request: %s" url);
  let body =
    let headers = Cohttp.Header.init_with "Cookie" ("session=" ^ token) in
    let open Lwt.Infix in
    Cohttp_lwt_unix.Client.get (Uri.of_string url) ~headers
    >>= fun (resp, body) ->
    let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
    match code with
    | 200 -> body |> Cohttp_lwt.Body.to_string
    | i -> Lwt.fail (HTTP_Error i)
  in
  let execute () =
    match Lwt_main.run body with
    | body -> Ok body
    | exception HTTP_Error 500 ->
        Error (`Msg (C.red "Error 500" ^ ": failed to fetch input."))
  in
  Bench.print_timings ~name:"fetch input" execute

let fetch_input ~year ~day ~file () =
  let url = Fmt.str "%s/%d/day/%d/input" base_url year day in
  let* token = get_token () in
  let* input = api_request ~url ~token () in
  let* _ = ensuredir file in
  Bos.OS.File.write file input

let get_input ~sw ~fs ~year ~day () =
  let fname =
    Fpath.(v root / "input" / string_of_int year / string_of_int day)
  in
  let* exists = Bos.OS.Path.exists fname in
  let+ () = if exists then Ok () else fetch_input ~year ~day ~file:fname () in
  let p = Eio.Path.(fs / Fpath.to_string fname) in
  (Eio.Path.open_in ~sw p :> Eio.Flow.source)
