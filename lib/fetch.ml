exception HTTP_Error of int

let src = Logs.Src.create "aoc.fetch"

let api_request ~url ~token () =
  Logs.info ~src (fun fmt -> fmt "Performing request: %s" url);
  let body =
    let headers = Cohttp.Header.init_with "Cookie" ("session=" ^ token) in
    let headers =
      Cohttp.Header.add headers "User-Agent"
        "github.com/TheLortex/aocaml by lucas@pluvina.ge"
    in
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
