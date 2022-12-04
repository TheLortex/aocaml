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

module Misc = Misc

let solve ~input ~name solver pp =
  Bench.print_timings ~bench:1 ~name ~pp_output:pp (fun () -> solver input)

let execute_result ~env ~stdin ~year (module S : Solution) =
  Fmt.pr "⁙ Day %s\n" (S.day |> string_of_int |> C.green |> C.bold);
  Eio.Switch.run @@ fun sw ->
  let+ input =
    match stdin with
    | false -> Storage.get_input ~sw ~env ~year ~day:S.day ()
    | true -> Ok (Eio.Stdenv.stdin env)
  in
  let input = Eio.Buf_read.of_flow input ~max_size:16000 in
  let parsed_input =
    Bench.print_timings ~name:"read input" (fun () -> S.parse_input input)
  in
  S.keys
  |> List.iter (fun x ->
         solve ~name:(S.pp_key x) ~input:parsed_input (S.eval x) S.format
         |> ignore)

let execute ~env ~stdin ~year (module S : Solution) =
  match execute_result ~env ~stdin ~year (module S) with
  | Ok () -> Fmt.pr "\n"
  | Error (`Msg str) -> Fmt.pr "Error: %s\n" str

let main ~env ~stdin ~year modules =
  Fmt.pr "\n%s\n\n%!" (C.green (C.bold " ⁘⁙⁘⁙⁘ Advent of Code! ⁘⁙⁘⁙⁘"));
  let _ =
    modules
    |> List.sort (fun (module S1 : Solution) (module S2 : Solution) ->
           S1.day - S2.day)
    |> List.map (execute ~env ~stdin ~year)
  in
  ()

let usage = "AOC: Advent of Code helper"
let last = ref false
let stdin = ref false

let args =
  Arg.parse
    [
      ("--last", Set last, "only run last day");
      ("--stdin", Set stdin, "read stdin instead of fetching input");
    ]
    (fun _ -> ())
    usage

let rec get_last = function
  | [ x ] -> x
  | _ :: q -> get_last q
  | [] -> failwith "Empty list."

let main ~year days =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  Eio_main.run @@ fun env ->
  Eztls.run env @@ fun tls ->
  let env =
    object
      method clock = env#clock
      method cwd = env#cwd
      method net = env#net
      method stdin = env#stdin
      method tls = tls
    end
  in
  match (!last, !stdin) with
  | false, false -> main ~env ~stdin:false ~year days
  | true, false -> main ~env ~stdin:false ~year [ get_last days ]
  | true, true -> main ~env ~stdin:true ~year [ get_last days ]
  | false, true -> failwith "Cannot use stdin when executing all days."
