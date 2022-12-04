val get :
  net:#Eio.Net.t ->
  tls:Eztls.t ->
  uri:Uri.t ->
  token:string ->
  (string, [> `Msg of string ]) result
