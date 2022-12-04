let max a b = if a < b then b else a

let print_timings ?(ofs = 1) ?(bench = 1) ~name ?pp_output f =
  let t0 = Mtime_clock.now () in
  let res =
    for i = 1 to bench - 1 do
      f ()
    done;
    f ()
  in
  let t1 = Mtime_clock.now () in
  let elapsed = Mtime.span t0 t1 in
  let text =
    Fmt.str "%s⇒ %s \x1B[90m(%a)\x1B[0m" (String.make ofs ' ') name
      (Misc.pp_span ~factor:(Float.of_int bench))
      elapsed
  in
  let length =
    Uuseg_string.fold_utf_8 `Grapheme_cluster (fun x _ -> x + 1) 0 text
  in
  let dots = String.make (max 0 (40 - length)) '.' in
  let () =
    match pp_output with
    | Some pp -> Fmt.pr "%s %s \x1B[1m%a\x1B[0m\n" text dots pp res
    | None -> Fmt.pr "%s\n" text
  in
  res