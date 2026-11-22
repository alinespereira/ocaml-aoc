open Core

type history = int list
and histories = history list [@@deriving show]

let parse_input (input : string) : int list list =
  let parse_line (line : string) : int list =
    line |> String.split ~on:' ' |> List.map ~f:Int.of_string
  in
  input |> String.split_lines |> List.map ~f:parse_line
;;

let differentiate_once (data : int list) : int list =
  List.map2_exn (List.tl_exn data) (List.drop_last_exn data) ~f:( - )
;;

let differentiate (data : int list) : int list list =
  let rec loop (acc : int list list) : int list list =
    match acc with
    | curr :: _ when List.for_all ~f:(Int.equal 0) curr -> acc
    | curr :: _ -> loop (differentiate_once curr :: acc)
    | [] -> acc
  in
  loop [ data ]
;;

let forward (differences : int list list) : int =
  differences |> List.fold ~init:0 ~f:(fun acc curr -> acc + List.last_exn curr)
;;

let backward (differences : int list list) : int =
  differences |> List.fold ~init:0 ~f:(fun acc curr -> List.hd_exn curr - acc)
;;

let solve_part_one (input : string) : string =
  input
  |> parse_input
  |> List.map ~f:differentiate
  |> List.map ~f:forward
  |> List.sum (module Int) ~f:Fun.id
  |> Int.to_string
;;

let solve_part_two (input : string) : string =
  input
  |> parse_input
  |> List.map ~f:differentiate
  |> List.map ~f:backward
  |> List.sum (module Int) ~f:Fun.id
  |> Int.to_string
;;

let%expect_test "AoC 2023: Day 09" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  Printf.printf "Part 1: %s\n" (solve_part_one input);
  [%expect {| Part 1: 114 |}];
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: 2 |}]
;;
