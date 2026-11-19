open Base
open Stdlib
open Aoc.Common

let is_digit ch =
  let code = Char.code ch in
  Char.code '0' <= code && code <= Char.code '9'
;;

let numbers =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
;;

let as_digit = function
  | "one" -> Some "1"
  | "two" -> Some "2"
  | "three" -> Some "3"
  | "four" -> Some "4"
  | "five" -> Some "5"
  | "six" -> Some "6"
  | "seven" -> Some "7"
  | "eight" -> Some "8"
  | "nine" -> Some "9"
  | _ -> None
;;

(* Str.string_match (Str.regexp "nine") s 0;; *)

let replace_numbers s =
  let rec loop s acc =
    if s = ""
    then acc
    else (
      let rest, piece =
        match
          List.find_map
            (fun num ->
               let init, rest = String.cut_first (String.length num) s in
               Option.map (fun digit -> rest, digit) (as_digit init))
            numbers
        with
        | Some found -> found
        | None ->
          let init, rest = String.cut_first 1 s in
          rest, init
      in
      loop rest (acc ^ piece))
  in
  loop s ""
;;

let sum_calibrations calibrations =
  calibrations
  |> List.map (List.filter is_digit)
  |> List.filter_map (function
    | [] -> None
    | first :: _ as digits ->
      Some (Printf.sprintf "%c%c" first (List.hd (List.rev digits))))
  |> List.map int_of_string
  |> List.fold_left ( + ) 0
;;

let solve_part_one (input : string) : string =
  input
  |> Input.split_lines
  |> List.map String.to_seq
  |> List.map List.of_seq
  |> sum_calibrations
  |> string_of_int
;;

let solve_part_two (input : string) : string =
  input
  |> Input.split_lines
  |> List.map replace_numbers
  |> List.map String.to_seq
  |> List.map List.of_seq
  |> sum_calibrations
  |> string_of_int
;;

let%expect_test "AoC 2023: Day 01" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  Printf.printf "Part 1: %s\n" (solve_part_one input);
  [%expect {| Part 1: 142 |}];
  let input = Aoc.Common.Input.read_lines "test-2.txt" in
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: 281 |}]
;;
