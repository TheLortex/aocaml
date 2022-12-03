val print_timings :
  ?bench:int ->
  name:string ->
  ?pp_output:(Format.formatter -> 'a -> unit) ->
  (unit -> 'a) ->
  'a
