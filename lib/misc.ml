open Mtime.Span
open Mtime

let round x = floor (x +. 0.5)

let round_dfrac d x =
  (* rounds [x] to the [d]th decimal digit *)
  if x -. round x = 0. then x
  else
    (* x is an integer. *)
    let m = 10. ** float d in
    (* m moves 10^-d to 1. *)
    floor ((x *. m) +. 0.5) /. m

let pp_float_s ppf span =
  let m = abs_float span in
  if m < ms_to_s then
    (* m < 1ms, if <  100us, print us with 2 frac digit w.o. trailing zeros
                if >= 100us, print us without frac digit *)
    let us = span /. us_to_s in
    let us = if abs_float us < 100. then round_dfrac 2 us else round us in
    if abs_float us >= 1000. then Format.fprintf ppf "%gms" (copysign 1. us)
    else Format.fprintf ppf "%gμs" us
  else if m < 1. then
    (* m < 1s, if <  100ms, print ms with 2 frac digit w.o. trailing zeros
                if >= 100ms, print ms without frac digit *)
    let ms = span /. ms_to_s in
    let ms = if abs_float ms < 100. then round_dfrac 2 ms else round ms in
    if abs_float ms >= 1000. then Format.fprintf ppf "%gs" (copysign 1. ms)
    else Format.fprintf ppf "%gms" ms
  else if m < min_to_s then
    (* m < 1min, print [s] with 2 frac digit w.o. trailing zeros *)
    let s = round_dfrac 2 span in
    if abs_float s >= 60. then Format.fprintf ppf "%gmin" (copysign 1. s)
    else Format.fprintf ppf "%gs" s
  else if
    (* m >= 1min
        From here on we show the two (or one if the second is zero) largest
        significant units and no longer care about rounding the lowest unit,
        we just truncate. *)
    m < hour_to_s
  then
    let m, rem = (truncate (span /. min_to_s), mod_float span min_to_s) in
    let s = truncate rem in
    if s = 0 then Format.fprintf ppf "%dmin" m
    else Format.fprintf ppf "%dmin%ds" m (abs s)
  else if m < day_to_s then
    let h, rem = (truncate (span /. hour_to_s), mod_float span hour_to_s) in
    let m = truncate (rem /. min_to_s) in
    if m = 0 then Format.fprintf ppf "%dh" h
    else Format.fprintf ppf "%dh%dmin" h (abs m)
  else if m < year_to_s then
    let d, rem = (truncate (span /. day_to_s), mod_float span day_to_s) in
    let h = truncate (rem /. hour_to_s) in
    if h = 0 then Format.fprintf ppf "%dd" d
    else Format.fprintf ppf "%dd%dh" d (abs h)
  else
    let y, rem = (truncate (span /. year_to_s), mod_float span year_to_s) in
    let d = truncate (rem /. day_to_s) in
    if d = 0 then Format.fprintf ppf "%da" y
    else Format.fprintf ppf "%da%dd" y (abs d)

let pp_span ?(factor = 1.) ppf s = pp_float_s ppf (to_s s /. factor)

module DefaultIntSolution = struct
  type solution = int

  type key = P1 | P2

  let keys = [ P1; P2 ]

  let pp_key = function P1 -> "part 1" | P2 -> "part 2"

  let format fmt = Fmt.pf fmt "%d"
end

module DefaultStringSolution = struct
  type solution = string

  type key = P1 | P2

  let keys = [ P1; P2 ]

  let pp_key = function P1 -> "part 1" | P2 -> "part 2"

  let format fmt = Fmt.pf fmt "%s"
end
