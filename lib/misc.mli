val pp_span : Mtime.Span.t Fmt.t

module DefaultIntSolution : sig
  type solution = int

  type key = P1 | P2

  val keys : key list

  val pp_key : key -> string

  val format : solution Fmt.t
end
