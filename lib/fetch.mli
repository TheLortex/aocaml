val get :
  net:#Eio.Net.t ->
  fs:#Eio.Fs.dir Eio.Path.t ->
  uri:Uri.t ->
  token:string ->
  (string, [> `Msg of string ]) result
