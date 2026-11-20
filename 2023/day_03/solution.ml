open Aoc.Common

type number =
  { pos : position
  ; len : int
  ; value : int
  }
[@@deriving show]

and position =
  { row : int
  ; col : int
  }
[@@deriving show]

let parse_line (row : int) (input : string) : number list =
  let input =
    input |> String.to_seq |> Seq.mapi (fun i c -> i, c) |> List.of_seq
  in
  let rec loop input acc =
    match input with
    | [] -> acc
    | _ ->
      let input =
        input |> List.drop_while (fun (i, c) -> not (Predicate.is_digit c))
      in
      let digits =
        input |> List.take_while (fun (i, c) -> Predicate.is_digit c)
      in
      let len = List.length digits in
      if len <= 0
      then acc
      else (
        let value =
          digits
          |> List.map snd
          |> List.to_seq
          |> String.of_seq
          |> int_of_string
        in
        let col = digits |> List.map fst |> List.hd in
        let acc = { pos = { row; col }; value; len } :: acc in
        let input =
          input |> List.drop_while (fun (i, c) -> Predicate.is_digit c)
        in
        loop input acc)
  in
  loop input []
;;

let parse_numbers (input : string) : number list =
  let lines = Input.split_lines input in
  List.mapi parse_line lines |> List.concat
;;

let find_symbols (input : string) : position list =
  Input.split_lines input
  |> List.map String.to_seq
  |> List.map List.of_seq
  |> List.mapi (fun row chars ->
    List.mapi (fun col ch -> ch, { row; col }) chars)
  |> List.concat
  |> List.filter_map (function
    | ch, _ when Predicate.is_digit ch || ch = '.' -> None
    | _, pos -> Some pos)
;;

let parse_engine (input : string) : number list * position list =
  parse_numbers input, find_symbols input
;;

let expand_pos ({ pos; len; _ } : number) : position list =
  List.init len Fun.id |> List.map (fun dx -> { pos with col = pos.col + dx })
;;

let up ({ row; col } : position) : position = { row = row - 1; col }
let down ({ row; col } : position) : position = { row = row + 1; col }
let left ({ row; col } : position) : position = { row; col = col - 1 }
let right ({ row; col } : position) : position = { row; col = col + 1 }

let neighbourhood (pos : position) : position list =
  [ pos |> up |> left
  ; pos |> up
  ; pos |> up |> right
  ; pos |> left
  ; pos |> right
  ; pos |> down |> left
  ; pos |> down
  ; pos |> down |> right
  ]
;;

let adjacent (number : number) (symbol : position) : bool =
  List.exists
    (fun neighbour -> List.mem neighbour (expand_pos number))
    (neighbourhood symbol)
;;

let solve_part_one (input : string) : string =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  let numbers, symbols = parse_engine input in
  numbers
  |> List.filter (fun number -> List.exists (adjacent number) symbols)
  |> List.map (fun { value; _ } -> value)
  |> List.fold_left ( + ) 0
  |> string_of_int
;;

let solve_part_two (_input : string) : string = failwith "TODO"

let%expect_test "AoC 2023: Day 03 - parse_input" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  parse_numbers input
  |> List.map show_number
  |> String.concat "\n"
  |> print_endline;
  [%expect
    {|
    { Solution.pos = { Solution.row = 0; col = 5 }; len = 3; value = 114 }
    { Solution.pos = { Solution.row = 0; col = 0 }; len = 3; value = 467 }
    { Solution.pos = { Solution.row = 2; col = 6 }; len = 3; value = 633 }
    { Solution.pos = { Solution.row = 2; col = 2 }; len = 2; value = 35 }
    { Solution.pos = { Solution.row = 4; col = 0 }; len = 3; value = 617 }
    { Solution.pos = { Solution.row = 5; col = 7 }; len = 2; value = 58 }
    { Solution.pos = { Solution.row = 6; col = 2 }; len = 3; value = 592 }
    { Solution.pos = { Solution.row = 7; col = 6 }; len = 3; value = 755 }
    { Solution.pos = { Solution.row = 9; col = 5 }; len = 3; value = 598 }
    { Solution.pos = { Solution.row = 9; col = 1 }; len = 3; value = 664 }
    |}]
;;

let%expect_test "AoC 2023: Day 03 - expand" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  parse_numbers input
  |> List.hd
  |> expand_pos
  |> List.map show_position
  |> String.concat "\n"
  |> print_endline;
  [%expect
    {|
    { Solution.row = 0; col = 5 }
    { Solution.row = 0; col = 6 }
    { Solution.row = 0; col = 7 }    
    |}]
;;

let%expect_test "AoC 2023: Day 03 - find_symbols" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  find_symbols input
  |> List.map show_position
  |> String.concat "\n"
  |> print_endline;
  [%expect
    {|
    { Solution.row = 1; col = 3 }
    { Solution.row = 3; col = 6 }
    { Solution.row = 4; col = 3 }
    { Solution.row = 5; col = 5 }
    { Solution.row = 8; col = 3 }
    { Solution.row = 8; col = 5 }
    |}]
;;

let%expect_test "AoC 2023: Day 03" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  Printf.printf "Part 1: %s\n" (solve_part_one input);
  [%expect {| Part 1: 4361 |}];
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: 467835 |}]
;;
