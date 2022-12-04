type t = { tls_config : Tls.Config.client }

let run env fn =
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let authenticator =
    X509_eio.authenticator
      (`Ca_dir Eio.Path.(env#fs / "/etc" / "ssl" / "certs"))
  in
  let tls_config = Tls.Config.client ~authenticator () in
  fn { tls_config }

let client_of_flow t ~host conn =
  Tls_eio.client_of_flow
    ~host:(Domain_name.of_string_exn host |> Domain_name.host_exn)
    t.tls_config conn
