module type Solution = sig
  type t

  val day : int
  val parse_input : Eio.Buf_read.t -> t
  val part_1 : t -> int
  val part_2 : t -> int
end

val main : year:int -> (module Solution) list -> unit
