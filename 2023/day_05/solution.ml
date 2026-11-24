open Core
open Aoc.Common

type interval =
  { start : int
  ; length : int
  }
[@@deriving show]

type intervals = interval list [@@deriving show]

type range =
  { destination : int
  ; source : interval
  }
[@@deriving show]

type map = { ranges : range list } [@@deriving show]

let parse_range (line : string) : range =
  match String.split ~on:' ' line |> List.map ~f:int_of_string with
  | [ destination; start; length ] ->
    { destination; source = { start; length } }
  | _ -> invalid_arg "input does not match expected format"
;;

let parse_map (lines : string list) : string * map =
  match lines with
  | label :: maps ->
    let label = label |> String.split ~on:' ' |> List.hd_exn in
    label, { ranges = List.map ~f:parse_range maps }
  | _ -> invalid_arg "input does not match expected format"
;;

let parse_seed_list (line : string) : int list =
  match String.split ~on:' ' line with
  | _ :: seeds -> List.map ~f:int_of_string seeds
  | _ -> invalid_arg "parse_seed_list: input does not match expected format"
;;

let rec parse_seed_intervals (seeds : int list) : intervals =
  match seeds with
  | start :: length :: rest -> { start; length } :: parse_seed_intervals rest
  | [] -> []
  | _ ->
    invalid_arg "parse_seed_intervals: input does not match expected format"
;;

let parse_input (input : string) : int list * (string * map) list =
  let lines = input |> String.split ~on:'\n' in
  match
    List.fold_right
      ~f:(fun line acc ->
        match acc with
        | _ when String.length line = 0 -> [] :: acc
        | x :: rest -> (line :: x) :: rest
        | [] -> [ line ] :: acc)
      ~init:[]
      lines
  with
  | [ seeds ] :: maps ->
    let seeds = parse_seed_list seeds in
    let maps = List.map ~f:parse_map maps in
    seeds, maps
  | _ -> invalid_arg "input does not match expected format"
;;

let peek (seed : int) ({ destination; source = { start; length } } : range)
  : int option
  =
  if start <= seed && seed < start + length
  then Some (destination + (seed - start))
  else None
;;

let step (seed : int) ({ ranges; _ } : map) : int =
  List.find_map ~f:(peek seed) ranges |> Option.value ~default:seed
;;

let traverse (seed : int) (maps : map list) : int =
  List.fold_left ~f:step ~init:seed maps
;;

let solve_part_one (input : string) : string =
  let seeds, maps = parse_input input in
  let maps = List.unzip maps |> snd in
  seeds
  |> List.map ~f:(fun seed -> traverse seed maps)
  |> List.fold_left ~f:min ~init:Int.max_value
  |> string_of_int
;;

let interval_end ({ start; length } : interval) = start + length

let intersect_interval (this : interval) (other : interval) : interval option =
  let this_end = interval_end this in
  let other_end = interval_end other in
  let { start = this_start; length = this_length } = this in
  let { start = other_start; length = other_length } = other in
  let result_start = max this_start other_start in
  let result_end = min this_end other_end in
  let result_length = result_end - result_start in
  if result_length <= 0
  then None
  else Some { start = result_start; length = result_length }
;;

let difference_interval (this : interval) (other : interval) : intervals =
  match intersect_interval this other with
  | None -> [ other ]
  | Some inter ->
    let other_stop = other.start + other.length in
    let inter_stop = inter.start + inter.length in
    let left =
      if other.start < inter.start
      then [ { start = other.start; length = inter.start - other.start } ]
      else []
    in
    let right =
      if inter_stop < other_stop
      then [ { start = inter_stop; length = other_stop - inter_stop } ]
      else []
    in
    left @ right
;;

let displace_interval (destination : int) ({ length; _ } : interval) : interval =
  { start = destination; length }
;;

(* Maps the interval [start, start + length) through one range. Returns the
   shifted part that falls inside the range, if any, plus the leftover pieces
   on either side, which still have to be tried against the other ranges. *)
let split_interval (interval : interval) ({ destination; source } : range)
  : interval option * intervals
  =
  match intersect_interval interval source with
  | None -> None, [ interval ]
  | intersected ->
    let mapped = Option.map ~f:(displace_interval destination) intersected in
    let unmapped = difference_interval interval source in
    mapped, unmapped
;;

(* Like [step], but for intervals: pieces that hit a range are shifted and
   leave the pending list, so no other range of the same map touches them;
   pieces that hit no range pass through unchanged. *)
let step_intervals (intervals : intervals) ({ ranges } : map) : intervals =
  let mapped, unmapped =
    List.fold_left
      ~f:(fun (mapped, pending) range ->
        List.fold_left
          ~f:(fun (mapped, rest) interval ->
            match split_interval interval range with
            | Some hit, leftover -> hit :: mapped, leftover @ rest
            | None, leftover -> mapped, leftover @ rest)
          ~init:(mapped, [])
          pending)
      ~init:([], intervals)
      ranges
  in
  mapped @ unmapped
;;

let solve_part_two (input : string) : string =
  let seeds, maps = parse_input input in
  let maps = List.unzip maps |> snd in
  List.fold_left ~f:step_intervals ~init:(parse_seed_intervals seeds) maps
  |> List.fold_left
       ~f:(fun acc { start; _ } -> min acc start)
       ~init:Int.max_value
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
  let seed_to_soil =
    List.Assoc.find_exn maps ~equal:String.equal "seed-to-soil"
  in
  let soil_to_fertilizer =
    List.Assoc.find_exn maps ~equal:String.equal "soil-to-fertilizer"
  in
  let fertilizer_to_water =
    List.Assoc.find_exn maps ~equal:String.equal "fertilizer-to-water"
  in
  let water_to_light =
    List.Assoc.find_exn maps ~equal:String.equal "water-to-light"
  in
  let light_to_temperature =
    List.Assoc.find_exn maps ~equal:String.equal "light-to-temperature"
  in
  let temperature_to_humidity =
    List.Assoc.find_exn maps ~equal:String.equal "temperature-to-humidity"
  in
  let humidity_to_location =
    List.Assoc.find_exn maps ~equal:String.equal "humidity-to-location"
  in
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
