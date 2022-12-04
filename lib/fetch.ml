let ( let* ) = Rresult.( >>= )

exception HTTP_Error of int

let src = Logs.Src.create "aoc.fetch"

let headers ~token =
  Http.Header.of_list
    [
      ("Cookie", "session=" ^ token);
      ("User-Agent", "github.com/TheLortex/aocaml by lucas@pluvina.ge");
    ]

let authenticator fs =
  X509_eio.authenticator (`Ca_dir Eio.Path.(fs / "/etc" / "ssl" / "certs"))

let get ~env ~uri ~token =
  let headers = headers ~token in
  Logs.info ~src (fun fmt -> fmt "Performing request: %a" Uri.pp uri);
  let host = Uri.host uri |> Option.get in
  Eio.Net.with_tcp_connect ~host ~service:(Uri.scheme uri |> Option.get) env#net
  @@ fun conn ->
  Logs.info (fun f -> f "TCP connected");
  let conn = Eztls.client_of_flow env#tls ~host conn in
  Logs.info (fun f -> f "TLS connected");
  let http_response, reader =
    Cohttp_eio.Client.get ~headers ~conn (host, None) (Uri.path_and_query uri)
  in
  match Http.Response.status http_response with
  | `OK ->
      Eio.Flow.shutdown conn `Send;
      Ok (Cohttp_eio.Client.read_fixed (http_response, reader))
  | t -> Error (`Msg ("Error " ^ Http.Status.to_string t))
