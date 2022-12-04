type t

val run :
  < clock : Eio.Time.clock
  ; fs : #Eio.Fs.dir Eio.Path.t
  ; secure_random : Eio.Flow.source
  ; .. > ->
  (t -> 'a) ->
  'a

val client_of_flow : t -> host:string -> #Eio.Flow.two_way -> Tls_eio.t
