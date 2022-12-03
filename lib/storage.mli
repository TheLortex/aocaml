val get_input :
  sw:Eio.Switch.t ->
  fs:#Eio.Fs.dir Eio.Path.t ->
  year:int -> day:int -> unit -> (Eio.Flow.source, [> Rresult.R.msg ]) result
