open Eio
include Aoc.Misc.DefaultIntSolution

let day = 19

type resources = { ore : int; clay : int; obsidian : int; geode : int }

let nothing = { ore = 0; clay = 0; obsidian = 0; geode = 0 }

type blueprint = {
  id : int;
  ore_robot_cost : resources;
  clay_robot_cost : resources;
  obsidian_robot_cost : resources;
  geode_robot_cost : resources;
}

type t = blueprint list

let parse_number b =
  let n =
    Eio.Buf_read.take_while1
      (function '0' .. '9' | '-' -> true | _ -> false)
      b
  in
  int_of_string n

let parse_blueprint i =
  let open Buf_read in
  string "Blueprint " i;
  let id = parse_number i in
  string ": Each ore robot costs " i;
  let ore_cost_ore = parse_number i in
  string " ore. Each clay robot costs " i;
  let clay_cost_ore = parse_number i in
  string " ore. Each obsidian robot costs " i;
  let obsidian_cost_ore = parse_number i in
  string " ore and " i;
  let obsidian_cost_clay = parse_number i in
  string " clay. Each geode robot costs " i;
  let geode_cost_ore = parse_number i in
  string " ore and " i;
  let geode_cost_obsidian = parse_number i in
  string " obsidian.\n" i;
  {
    id;
    ore_robot_cost = { nothing with ore = ore_cost_ore };
    clay_robot_cost = { nothing with ore = clay_cost_ore };
    obsidian_robot_cost =
      { nothing with ore = obsidian_cost_ore; clay = obsidian_cost_clay };
    geode_robot_cost =
      { nothing with ore = geode_cost_ore; obsidian = geode_cost_obsidian };
  }

let parse_input input = Buf_read.seq parse_blueprint input |> List.of_seq

type state = {
  blueprint : blueprint;
  resources : resources;
  robots : resources;
  time_left : int;
}

let n_turns_to_get quantity production =
  if production = 0 then if quantity > 0 then None else Some 0
  else Some (max 0 ((quantity + production - 1) / production))

let advance turns st =
  {
    st with
    time_left = st.time_left - turns;
    resources =
      {
        ore = st.resources.ore + (st.robots.ore * turns);
        clay = st.resources.clay + (st.robots.clay * turns);
        obsidian = st.resources.obsidian + (st.robots.obsidian * turns);
        geode = st.resources.geode + (st.robots.geode * turns);
      };
  }

let pay cost st =
  {
    st with
    resources =
      {
        ore = st.resources.ore - cost.ore;
        clay = st.resources.clay - cost.clay;
        obsidian = st.resources.obsidian - cost.obsidian;
        geode = st.resources.geode - cost.geode;
      };
  }

let wait_for st cost =
  let ( let* ) = Option.bind in
  let ore_needed = cost.ore - st.resources.ore in
  let obsidian_needed = cost.obsidian - st.resources.obsidian in
  let clay_needed = cost.clay - st.resources.clay in
  let* ore_turns = n_turns_to_get ore_needed st.robots.ore in
  let* obsidian_turns = n_turns_to_get obsidian_needed st.robots.obsidian in
  let* clay_turns = n_turns_to_get clay_needed st.robots.clay in
  let turns = max ore_turns (max obsidian_turns clay_turns) in
  if turns < st.time_left then Some (advance (turns + 1) st |> pay cost)
  else None

let build_ore st =
  { st with robots = { st.robots with ore = st.robots.ore + 1 } }

let build_clay st =
  { st with robots = { st.robots with clay = st.robots.clay + 1 } }

let build_obsidian st =
  { st with robots = { st.robots with obsidian = st.robots.obsidian + 1 } }

let build_geode st =
  { st with robots = { st.robots with geode = st.robots.geode + 1 } }

let max_cost bp =
  let max a b c d = max (max a b) (max c d) in
  {
    ore =
      max bp.ore_robot_cost.ore bp.clay_robot_cost.ore
        bp.obsidian_robot_cost.ore bp.geode_robot_cost.ore;
    clay =
      max bp.ore_robot_cost.clay bp.clay_robot_cost.clay
        bp.obsidian_robot_cost.clay bp.geode_robot_cost.clay;
    obsidian =
      max bp.ore_robot_cost.obsidian bp.clay_robot_cost.obsidian
        bp.obsidian_robot_cost.obsidian bp.geode_robot_cost.obsidian;
    geode =
      max bp.ore_robot_cost.geode bp.clay_robot_cost.geode
        bp.obsidian_robot_cost.geode bp.geode_robot_cost.geode;
  }

let actions st =
  let max_cost = max_cost st.blueprint in
  let ore =
    if max_cost.ore > st.robots.ore then
      wait_for st st.blueprint.ore_robot_cost |> Option.map build_ore
    else None
  in
  let obsidian =
    if max_cost.obsidian > st.robots.obsidian then
      wait_for st st.blueprint.obsidian_robot_cost |> Option.map build_obsidian
    else None
  in
  let clay =
    if max_cost.clay > st.robots.clay then
      wait_for st st.blueprint.clay_robot_cost |> Option.map build_clay
    else None
  in
  let geode =
    wait_for st st.blueprint.geode_robot_cost |> Option.map build_geode
  in
  let wait = advance st.time_left st in
  match [ ore; obsidian; clay; geode ] |> List.filter_map Fun.id with
  | [] -> [ wait ]
  | v -> v

let pp t =
  Printf.printf "[%02d] %3d %3d %3d %3d\n%!" t.time_left t.resources.ore
    t.resources.clay t.resources.obsidian t.resources.geode;
  Printf.printf "[  ] %3d %3d %3d %3d\n\n%!" t.robots.ore t.robots.clay
    t.robots.obsidian t.robots.geode

let max_value st =
  st.resources.geode
  + (st.robots.geode * st.time_left)
  + (st.time_left * (st.time_left - 1) / 2)

let rec explore ~best st =(*
  if st.time_left = 12 then pp st; *)
  if best > max_value st then best
  else if st.time_left = 0 then st.resources.geode
  else
    let actions = actions st in
    List.fold_left
      (fun best act ->
        let v = explore ~best act in
        max v best)
      best actions

let part_1 t =
  t
  |> List.map (fun blueprint ->
         explore ~best:0
           {
             time_left = 24;
             blueprint;
             resources = nothing;
             robots = { nothing with ore = 1 };
           }
         * blueprint.id)
  |> List.fold_left ( + ) 0

let take_3 = function a :: b :: c :: _ -> [ a; b; c ] | l -> l

let part_2 t =
  t |> take_3
  |> List.map (fun blueprint ->
         explore ~best:0
           {
             time_left = 32;
             blueprint;
             resources = nothing;
             robots = { nothing with ore = 1 };
           })
  |> List.fold_left ( * ) 1

let eval = function P1 -> part_1 | P2 -> part_2
