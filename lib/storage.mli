val get_input :
  env:< cwd : #Eio.Fs.dir Eio.Path.t ; tls : Eztls.t ; net : Eio.Net.t ; .. > ->
  sw:Eio.Switch.t ->
  year:int ->
  day:int ->
  unit ->
  (Eio.Flow.source, [> Rresult.R.msg ]) result
