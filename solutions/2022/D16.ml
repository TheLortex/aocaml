open Eio
include Aoc.Misc.DefaultIntSolution

let day = 16

type report = { valve : string; flow : int; edges : string list }
type t = report list

let parse_number b =
  let n =
    Eio.Buf_read.take_while1
      (function '0' .. '9' | '-' -> true | _ -> false)
      b
  in
  int_of_string n

let maybe_s i =
  match Buf_read.peek_char i with Some 's' -> Buf_read.char 's' i | _ -> ()

let parse_report i =
  Buf_read.string "Valve " i;
  let valve = Buf_read.take_while (( <> ) ' ') i in
  Buf_read.string " has flow rate=" i;
  let flow = parse_number i in
  Buf_read.string "; tunnel" i;
  maybe_s i;
  Buf_read.string " lead" i;
  maybe_s i;
  Buf_read.string " to valve" i;
  maybe_s i;
  Buf_read.char ' ' i;
  let edges = Buf_read.line i |> Astring.String.cuts ~sep:", " in
  { valve; flow; edges }

let parse_input input = Eio.Buf_read.seq parse_report input |> List.of_seq

type valve = { valve : int; flow : int; edges : int list }

module Index = struct
  type t = (string, int) Hashtbl.t

  let init () = Hashtbl.create 10
  let add t idx ({ valve; _ } : report) = Hashtbl.add t valve idx
  let find t v = Hashtbl.find t v

  let convert t (r : report) =
    {
      valve = Hashtbl.find t r.valve;
      flow = r.flow;
      edges = List.map (Hashtbl.find t) r.edges;
    }
end

module Paths = struct
  type t = int array array

  let make valves =
    let count = Array.length valves in
    let result = Array.make_matrix count count 999 in
    for i = 0 to count - 1 do
      result.(i).(i) <- 0
    done;
    for st = 0 to count - 1 do
      for k = 0 to count - 1 do
        for s = 0 to count - 1 do
          let node = valves.(s) in
          List.iter
            (fun e ->
              if result.(st).(s) + 1 < result.(st).(e) then
                result.(st).(e) <- result.(st).(s) + 1)
            node.edges
        done
      done
    done;
    result
end

module State = struct
  type t = {
    map : valve array;
    paths : Paths.t;
    position : int;
    is_open : int;
    time_left : int;
    current_flow_rate : int;
    total_flow : int;
  }

  let init ~time_left map position =
    {
      map;
      paths = Paths.make map;
      position;
      is_open = 0;
      time_left;
      current_flow_rate = 0;
      total_flow = 0;
    }

  let debug st =
    Printf.printf "%02d POS: %d | OPEN: %016x | %d / %d\n" st.time_left
      st.position st.is_open st.current_flow_rate st.total_flow

  let wait_until_end state =
    (state.time_left * state.current_flow_rate) + state.total_flow

  let valve_is_closed state p = state.is_open land (1 lsl p.valve) == 0

  let valve_open state p =
    {
      state with
      is_open = state.is_open lor (1 lsl p.valve);
      time_left = state.time_left - 1;
      total_flow = state.total_flow + state.current_flow_rate;
      current_flow_rate = state.current_flow_rate + p.flow;
    }

  let posible_actions state =
    Array.to_seq state.map
    |> Seq.filter (fun p ->
           p.flow > 0 && valve_is_closed state p
           && state.paths.(state.position).(p.valve) < state.time_left)

  let go_to_and_open state v =
    let distance = state.paths.(state.position).(v.valve) in
    let st_moved =
      {
        state with
        time_left = state.time_left - distance;
        position = v.valve;
        total_flow = state.total_flow + (distance * state.current_flow_rate);
      }
    in
    valve_open st_moved v

  let rec algorithm state =
    (* debug state; *)
    match posible_actions state |> List.of_seq with
    | [] -> wait_until_end state
    | [ v ] -> algorithm (go_to_and_open state v)
    | options ->
        List.map (fun v -> algorithm (go_to_and_open state v)) options
        |> List.fold_left max 0

  let algorithm_memo state =
    let memo = Hashtbl.create 100 in
    let rec loop state =
      (let result = wait_until_end state in
       match Hashtbl.find_opt memo state.is_open with
       | Some v when v > result -> ()
       | _ -> Hashtbl.replace memo state.is_open result);
      (* debug state; *)
      match posible_actions state |> List.of_seq with
      | [] -> ()
      | [ v ] -> loop (go_to_and_open state v)
      | options -> List.iter (fun v -> loop (go_to_and_open state v)) options
    in
    loop state;
    memo
end

let part_1 valves =
  let i = Index.init () in
  List.iteri (Index.add i) valves;
  let valves = List.map (Index.convert i) valves |> Array.of_list in
  let st = State.init ~time_left:30 valves (Index.find i "AA") in
  State.algorithm st

let part_2 valves =
  let i = Index.init () in
  List.iteri (Index.add i) valves;
  let valves = List.map (Index.convert i) valves |> Array.of_list in
  let st = State.init ~time_left:26 valves (Index.find i "AA") in
  let m = State.algorithm_memo st in
  let items =
    Hashtbl.to_seq m |> List.of_seq
    |> List.sort (fun (_, v1) (_, v2) -> v2 - v1)
  in
  List.map
    (fun (k, v) ->
      List.filter_map
        (fun (k2, v2) -> if k land k2 > 0 then None else Some (v + v2))
        items)
    items
  |> List.flatten |> List.fold_left max 0

let eval = function P1 -> part_1 | P2 -> part_2
