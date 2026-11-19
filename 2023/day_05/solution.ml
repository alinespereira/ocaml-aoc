open Base
open Stdlib
open Aoc.Common

type range =
  { destination : int
  ; source : int
  ; length : int
  }
[@@deriving show]

type map = { ranges : range list } [@@deriving show]

let parse_range (line : string) : range =
  match String.split_on_char ' ' line |> List.map int_of_string with
  | [ destination; source; length ] -> { destination; source; length }
  | _ -> invalid_arg "input does not match expected format"
;;

let parse_map (lines : string list) : string * map =
  match lines with
  | label :: maps ->
    let label = label |> String.split_on_char ' ' |> List.hd in
    label, { ranges = List.map parse_range maps }
  | _ -> invalid_arg "input does not match expected format"
;;

let parse_seed_list (line : string) : int list =
  match String.split_on_char ' ' line with
  | _ :: seeds -> List.map int_of_string seeds
  | _ -> invalid_arg "parse_seed_list: input does not match expected format"
;;

let rec parse_ranges_from_seed_list (seeds : int list) : int list =
  match seeds with
  | first :: length :: rest ->
    List.init length (fun i -> first + i) @ parse_ranges_from_seed_list rest
  | [] -> []
  | _ ->
    invalid_arg
      "parse_ranges_from_seed_list: input does not match expected format"
;;

let parse_input (input : string) : int list * (string * map) list =
  let lines = input |> String.split_on_char '\n' in
  match
    List.fold_right
      (fun line acc ->
         match acc with
         | _ when String.length line = 0 -> [] :: acc
         | x :: rest -> (line :: x) :: rest
         | [] -> [ line ] :: acc)
      lines
      []
  with
  | [ seeds ] :: maps ->
    let seeds = parse_seed_list seeds in
    let maps = List.map parse_map maps in
    seeds, maps
  | _ -> invalid_arg "input does not match expected format"
;;

let peek (seed : int) ({ destination; source; length } : range) : int option =
  if source <= seed && seed < source + length
  then Some (destination + (seed - source))
  else None
;;

let step (seed : int) ({ ranges; _ } : map) : int =
  List.find_map (peek seed) ranges |> Option.value ~default:seed
;;

let traverse (seed : int) (maps : map list) : int =
  List.fold_left step seed maps
;;

let solve_part_one (input : string) : string =
  let seeds, maps = parse_input input in
  let maps = List.split maps |> snd in
  seeds
  |> List.map (fun seed -> traverse seed maps)
  |> List.fold_left min Int.max_int
  |> string_of_int
;;

let solve_part_two (input : string) : string =
  let seeds, maps = parse_input input in
  let seeds = parse_ranges_from_seed_list seeds in
  let maps = List.split maps |> snd in
  seeds
  |> List.map (fun seed -> traverse seed maps)
  |> List.fold_left min Int.max_int
  |> string_of_int
;;

let%expect_test "AoC 2023: Day 05" =
  let input = Input.read_lines "test.txt" in
  Printf.printf "Part 1: %s\n" (solve_part_one input);
  [%expect {| Part 1: 35 |}];
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: 46 |}]
;;

let%expect_test "AoC 2023: Day 05 - steps" =
  let seed, soil, fertilizer, water, light, temperature, humidity, location =
    79, 81, 81, 81, 74, 78, 78, 82
  in
  let input = Input.read_lines "test.txt" in
  let _, maps = parse_input input in
  let seed_to_soil = List.assoc "seed-to-soil" maps in
  let soil_to_fertilizer = List.assoc "soil-to-fertilizer" maps in
  let fertilizer_to_water = List.assoc "fertilizer-to-water" maps in
  let water_to_light = List.assoc "water-to-light" maps in
  let light_to_temperature = List.assoc "light-to-temperature" maps in
  let temperature_to_humidity = List.assoc "temperature-to-humidity" maps in
  let humidity_to_location = List.assoc "humidity-to-location" maps in
  step seed seed_to_soil |> string_of_int |> print_endline;
  [%expect {| 81 |}];
  step soil soil_to_fertilizer |> string_of_int |> print_endline;
  [%expect {| 81 |}];
  step fertilizer fertilizer_to_water |> string_of_int |> print_endline;
  [%expect {| 81 |}];
  step water water_to_light |> string_of_int |> print_endline;
  [%expect {| 74 |}];
  step light light_to_temperature |> string_of_int |> print_endline;
  [%expect {| 78 |}];
  step temperature temperature_to_humidity |> string_of_int |> print_endline;
  [%expect {| 78 |}];
  step humidity humidity_to_location |> string_of_int |> print_endline;
  [%expect {| 82 |}]
;;
(* 14, 14, 53, 49, 42, 42, 43, 43; *)
(* 55, 57, 57, 53, 46, 82, 82, 86; *)
(* 13, 13, 52, 41, 34, 34, 35, 35; *)
