open Core
open Angstrom

type scratchcard =
  { winning : int list
  ; ours : int list
  }
[@@deriving show]

let whitespaces = take_while Char.is_whitespace
let digits = take_while1 Char.is_digit >>| Int.of_string

let card =
  let* _ =
    string "Card" *> whitespaces *> digits *> string ":" *> whitespaces
  in
  let* winning = sep_by1 whitespaces digits in
  let* _ = whitespaces *> string "|" <* whitespaces in
  let* ours = sep_by1 whitespaces digits in
  return { winning; ours }
;;

let parse_line (line : string) : scratchcard =
  match parse_string ~consume:All card (String.rstrip line) with
  | Ok v -> v
  | Error msg -> failwith msg
;;

let winning_numbers ({ winning; ours } : scratchcard) : int =
  let winning = Int.Set.of_list winning in
  let ours = Int.Set.of_list ours in
  Set.inter winning ours |> Set.length
;;

let score (card : scratchcard) : int =
  let num = winning_numbers card in
  if num = 0 then 0 else Int.pow 2 (num - 1)
;;

let unravel (copies : (int * int) list) : (int * int) list =
  let rec loop (copies : (int * int) list) (acc : (int * int) list) =
    match copies with
    | [] -> List.rev acc
    | curr :: rest ->
      let count, wins = curr in
      let extra, kept = List.split_n rest wins in
      let extra = List.map ~f:(fun (c, w) -> c + count, w) extra in
      loop (extra @ kept) (curr :: acc)
  in
  loop copies []
;;

let solve_part_one (input : string) : string =
  input
  |> String.split_lines
  |> List.map ~f:parse_line
  |> List.sum (module Int) ~f:score
  |> Int.to_string
;;

let solve_part_two (input : string) : string =
  input
  |> String.split_lines
  |> List.map ~f:parse_line
  |> List.map ~f:(fun card -> 1, winning_numbers card)
  |> unravel
  |> List.sum (module Int) ~f:fst
  |> Int.to_string
;;

let%expect_test "AoC 2023: Day 04 - parse_line" =
  let line = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" in
  let card = parse_line line in
  print_endline (show_scratchcard card);
  [%expect
    {|
    { Solution.winning = [41; 48; 83; 86; 17];
      ours = [83; 86; 6; 31; 17; 9; 48; 53] }
    |}]
;;

let%expect_test "AoC 2023: Day 04" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  Printf.printf "Part 1: %s\n" (solve_part_one input);
  [%expect {| Part 1: 13 |}];
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: 30 |}]
;;
