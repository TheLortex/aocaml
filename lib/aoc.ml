let ( let* ) = Rresult.( >>= )
let ( let+ ) = Rresult.( >>| )

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

let solve ~input ~name solver pp =
  Bench.print_timings ~bench:1 ~name ~pp_output:pp (fun () -> solver input)

let execute_result ~(stdenv : Eio.Stdenv.t) ~stdin ~year (module S : Solution) =
  Fmt.pr "⁙ Day %s\n" (S.day |> string_of_int |> C.green |> C.bold);
  let fs = Eio.Stdenv.cwd stdenv in
  Eio.Switch.run @@ fun sw ->
  let+ input =
    match stdin with
    | false -> Storage.get_input ~sw ~fs ~year ~day:S.day ()
    | true -> Ok (Eio.Stdenv.stdin stdenv)
  in
  let input = Eio.Buf_read.of_flow input ~max_size:16000 in
  let parsed_input =
    Bench.print_timings ~name:"read input" (fun () -> S.parse_input input)
  in
  S.keys
  |> List.iter (fun x ->
         solve ~name:(S.pp_key x) ~input:parsed_input (S.eval x) S.format
         |> ignore)

let execute ~stdenv ~stdin ~year (module S : Solution) =
  match execute_result ~stdenv ~stdin ~year (module S) with
  | Ok () -> Fmt.pr "\n"
  | Error (`Msg str) -> Fmt.pr "%s\n" str

let main ~stdenv ?(stdin = false) ~year modules =
  Fmt.pr "\n%s\n\n%!" (C.green (C.bold " ⁘⁙⁘⁙⁘ Advent of Code! ⁘⁙⁘⁙⁘"));
  let _ =
    modules
    |> List.sort (fun (module S1 : Solution) (module S2 : Solution) ->
           S1.day - S2.day)
    |> List.map (execute ~stdenv ~stdin ~year)
  in
  ()

module Misc = Misc
