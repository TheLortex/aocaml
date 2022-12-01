let ( let* ) = Rresult.( >>= )
let ( let+ ) = Rresult.( >>| )

module type Solution = sig
  type t
  type key
  type solution

  val pp_key : key -> string
  val keys : key list
  val day : int
  val parse_input : Eio.Buf_read.t -> t
  val eval : key -> t -> solution
  val format : solution Fmt.t
end

module C = struct
  let orange str = "\x1B[33m" ^ str ^ "\x1B[0m"
  let red str = "\x1B[31m" ^ str ^ "\x1B[0m"
  let green str = "\x1B[32m" ^ str ^ "\x1B[0m"
  let bold str = "\x1B[1m" ^ str ^ "\x1B[0m"
end

let ensuredir path =
  let parent, _ = Fpath.split_base path in
  Bos.OS.Dir.create ~path:true parent

let max a b = if a < b then b else a

let print_timings ?(bench = 1) ~name ?pp_output f =
  let t0 = Mtime_clock.now () in
  let res =
    for i = 1 to bench - 1 do
      f ()
    done;
    f ()
  in
  let t1 = Mtime_clock.now () in
  let elapsed = Mtime.span t0 t1 in
  let text =
    Fmt.str " ⇒ %s \x1B[90m(%a)\x1B[0m" name
      (Misc.pp_span ~factor:(Float.of_int bench))
      elapsed
  in
  let length =
    Uuseg_string.fold_utf_8 `Grapheme_cluster (fun x _ -> x + 1) 0 text
  in
  let dots = String.make (max 0 (40 - length)) '.' in
  let () =
    match pp_output with
    | Some pp -> Fmt.pr "%s %s \x1B[1m%a\x1B[0m\n" text dots pp res
    | None -> Fmt.pr "%s\n" text
  in
  res

module Storage = struct
  let root = "./data/"
  let base_url = "https://adventofcode.com"

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
    print_timings ~name:"fetch input" execute

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
end

let solve ~input ~name solver pp =
  print_timings ~bench:1 ~name ~pp_output:pp (fun () -> solver input)

let execute_result ~(stdenv : Eio.Stdenv.t) ~stdin ~year (module S : Solution) =
  Fmt.pr "⁙ Day %s\n" (S.day |> string_of_int |> C.green |> C.bold);
  let fs = Eio.Stdenv.cwd stdenv in
  Eio.Switch.run @@ fun sw ->
  let+ input =
    match stdin with
    | false -> Storage.get_input ~sw ~fs ~year ~day:S.day ()
    | true -> Ok (Eio.Stdenv.stdin stdenv)
  in
  let input = Eio.Buf_read.of_flow input ~max_size:16000 in
  let parsed_input =
    print_timings ~name:"read input" (fun () -> S.parse_input input)
  in
  S.keys
  |> List.iter (fun x ->
         solve ~name:(S.pp_key x) ~input:parsed_input (S.eval x) S.format
         |> ignore)

let execute ~stdenv ~stdin ~year (module S : Solution) =
  match execute_result ~stdenv ~stdin ~year (module S) with
  | Ok () -> Fmt.pr "\n"
  | Error (`Msg str) -> Fmt.pr "%s\n" str

let main ~stdenv ?(stdin = false) ~year modules =
  Fmt.pr "\n%s\n\n%!" (C.green (C.bold " ⁘⁙⁘⁙⁘ Advent of Code! ⁘⁙⁘⁙⁘"));
  let _ =
    modules
    |> List.sort (fun (module S1 : Solution) (module S2 : Solution) ->
           S1.day - S2.day)
    |> List.map (execute ~stdenv ~stdin ~year)
  in
  ()

module Misc = Misc
