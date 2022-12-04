let ( let* ) = Rresult.( >>= )
let ( let+ ) = Rresult.( >>| )

module type Solution = sig
  type t

  val day : int
  val parse_input : Eio.Buf_read.t -> t
  val part_1 : t -> int
  val part_2 : t -> int
end

let solve ~input ~name solver pp =
  Bench.print_timings ~ofs:2 ~bench:1 ~name ~pp_output:pp (fun () ->
      solver input)

let extract_example ~env ~sw ~year ~day =
  let* content = Storage.get_input ~sw ~env ~year ~day Page in
  let input = Eio.Buf_read.of_flow content ~max_size:16000 in
  let page = Eio.Buf_read.take_all input in
  let open Astring in
  match String.cut ~sep:"<pre><code>" page with
  | None -> Error (`Msg "failed to extract example (1)")
  | Some (_, a) -> (
      match String.cut ~sep:"</code></pre>" a with
      | None -> Error (`Msg "failed to extract example (2)")
      | Some (example, _) -> Ok example)

let solve (module S : Solution) input =
  let parsed_input =
    Bench.print_timings ~name:"read input" (fun () -> S.parse_input input)
  in
  solve ~name:"part 1" ~input:parsed_input S.part_1 Fmt.int |> ignore;
  solve ~name:"part 2" ~input:parsed_input S.part_2 Fmt.int |> ignore

let execute_result ~env ~stdin ~year (module S : Solution) =
  Fmt.pr "⁙ Day %s\n" (S.day |> string_of_int |> C.green |> C.bold);
  if stdin then
    Ok
      (solve
         (module S)
         (Eio.Stdenv.stdin env |> Eio.Buf_read.of_flow ~max_size:16000))
  else (
    Fmt.pr "\n⁙ Example\n";
    let* () =
      Eio.Switch.run (fun sw ->
          let+ example = extract_example ~env ~sw ~year ~day:S.day in
          solve (module S) (Eio.Buf_read.of_string example))
    in
    Fmt.pr "\n⁙ Input\n";
    Eio.Switch.run @@ fun sw ->
    let+ input = Storage.get_input ~sw ~env ~year ~day:S.day Input in
    let input = Eio.Buf_read.of_flow input ~max_size:16000 in
    solve (module S) input)

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
  | _, true -> main ~env ~stdin:true ~year [ get_last days ]
