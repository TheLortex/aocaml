val print_timings :
  ?ofs:int ->
  ?bench:int ->
  name:string ->
  ?pp_output:(Format.formatter -> 'a -> unit) ->
  (unit -> 'a) ->
  'a
