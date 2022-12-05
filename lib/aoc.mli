module type Solution = sig
  type t
  type key
  type solution

  val pp_key : key -> string
  val keys : key list
  val day : int
  val parse_input : Eio.Buf_read.t -> t
  val eval : key -> t -> solution
  val format : solution Fmt.t
end

module Misc = Misc

val main : year:int -> (module Solution) list -> unit
