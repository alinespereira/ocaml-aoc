open Base
open Stdlib
open Aoc.Common

type draw =
  { red : int
  ; green : int
  ; blue : int
  }

type game =
  { id : int
  ; draws : draw list
  }

let parse_draw (draw : string) : draw =
  let draw =
    String.split_all ~sep:", " draw
    |> List.filter_map (String.split_first ~sep:" ")
    |> List.map (fun (f, s) -> s, int_of_string f)
  in
  { red = List.assoc_opt "red" draw |> Option.value ~default:0
  ; green = List.assoc_opt "green" draw |> Option.value ~default:0
  ; blue = List.assoc_opt "blue" draw |> Option.value ~default:0
  }
;;

let parse_draws (draws : string) : draw list =
  let draws = String.split_all ~sep:"; " draws in
  List.map parse_draw draws
;;

let parse_game (game : string) : game =
  game
  |> String.split_first ~sep:": "
  |> Option.map (fun (game, draws) ->
    game
    |> String.split_last ~sep:" "
    |> Option.map (fun (_, id) ->
      { id = id |> int_of_string; draws = parse_draws draws })
    |> Option.get)
  |> Option.get
;;

let max_red = 12
let max_green = 13
let max_blue = 14

let valid_draw
      ?(max_red = max_red)
      ?(max_red = max_green)
      ?(max_red = max_blue)
      draw
  =
  let { red; green; blue } = draw in
  red <= max_red && green <= max_green && blue <= max_blue
;;

let possible_game
      ?(max_red = max_red)
      ?(max_red = max_green)
      ?(max_red = max_blue)
      game
  =
  let { draws; _ } = game in
  List.for_all valid_draw draws
;;

let minimum_set game =
  let { draws; _ } = game in
  { red =
      List.map (fun { red; _ } -> red) draws |> List.fold_left max Int.min_int
  ; green =
      List.map (fun { green; _ } -> green) draws
      |> List.fold_left max Int.min_int
  ; blue =
      List.map (fun { blue; _ } -> blue) draws |> List.fold_left max Int.min_int
  }
;;

let power game =
  let { red; green; blue } = minimum_set game in
  red * green * blue
;;

let parse_input (input : string) : game list =
  input |> Input.split_lines |> List.map parse_game
;;

let solve_part_one (input : string) : string =
  input
  |> parse_input
  |> List.filter possible_game
  |> List.map (fun { id; _ } -> id)
  |> List.fold_left ( + ) 0
  |> string_of_int
;;

let solve_part_two (input : string) : string =
  input
  |> parse_input
  |> List.map power
  |> List.fold_left ( + ) 0
  |> string_of_int
;;

let%expect_test "AoC 2023: Day 02" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  Printf.printf "Part 1: %s\n" (solve_part_one input);
  [%expect {| Part 1: 8 |}];
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: 2286 |}]
;;
