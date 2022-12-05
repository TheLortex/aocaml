val pp_span : ?factor:float -> Mtime.Span.t Fmt.t

module DefaultIntSolution : sig
  type solution = int

  type key = P1 | P2

  val keys : key list

  val pp_key : key -> string

  val format : solution Fmt.t
end

module DefaultStringSolution : sig
  type solution = string

  type key = P1 | P2

  val keys : key list

  val pp_key : key -> string

  val format : solution Fmt.t
end
