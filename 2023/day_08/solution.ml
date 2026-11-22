open Core
open Angstrom

type instructions = char list [@@deriving show]
type action = string * string -> string
type actions = action list
type nodes = (string * string * string) list [@@deriving show]
type network = (string * string) String.Map.t

let sep = take_while (fun c -> Char.equal c ' ' || Char.equal c '\t')
let instruction = char 'L' <|> char 'R'
let instructions = many1 instruction <* end_of_line

let node_label =
  count 3 @@ satisfy (fun ch -> Char.is_uppercase ch || Char.is_digit ch)
  >>| String.of_char_list
;;

let node =
  let* label = node_label in
  let* left = string " = (" *> node_label in
  let* right = string ", " *> node_label <* string ")" in
  return (label, left, right)
;;

let parser =
  let* is = instructions in
  let* _ = many1 (sep *> end_of_line) in
  let* ns = sep_by1 (sep *> end_of_line) node in
  return (is, ns)
;;

let parse_input (input : string) =
  let instructions, nodes =
    match parse_string ~consume:Prefix parser input with
    | Ok v -> v
    | Error err -> failwith err
  in
  instructions, nodes
;;

let as_network (nodes : nodes) : network =
  nodes
  |> List.fold
       ~f:(fun acc (label, left, right) ->
         Map.add_exn ~key:label ~data:(left, right) acc)
       ~init:String.Map.empty
;;

let as_actions (instructions : instructions) : actions =
  instructions
  |> List.map ~f:(function
    | 'L' -> fst
    | 'R' -> snd
    | ch -> Fmt.failwith "invalid instruction: %c" ch)
;;

let step (action : action) (network : network) (position : string) : string =
  Map.find_exn network position |> action
;;

let walk
      (actions : actions)
      (network : network)
      (origin : string)
      (cond : string -> bool)
  : int
  =
  actions
  |> Sequence.cycle_list_exn
  |> Sequence.fold_until
       ~init:(0, origin)
       ~f:(fun (steps, pos) action ->
         if cond pos
         then Stop steps
         else Continue (steps + 1, step action network pos))
       ~finish:fst
;;

let rec gcd a b = if b = 0 then a else gcd b (a % b)
let lcm a b = a / gcd a b * b

type node_list = string list [@@deriving show]

let solve_part_one (input : string) : string =
  let instructions, nodes = parse_input input in
  let actions = as_actions instructions in
  let network = as_network nodes in
  let origin = "AAA" in
  walk actions network origin (String.equal "ZZZ") |> Int.to_string
;;

let solve_part_two (input : string) : string =
  let instructions, nodes = parse_input input in
  let actions = as_actions instructions in
  let network = as_network nodes in
  let origins =
    Map.keys network
    |> List.filter ~f:(fun label ->
      String.to_list label |> List.last_exn |> Char.equal 'A')
  in
  let cond label = String.is_suffix label ~suffix:"Z" in
  origins
  |> List.map ~f:(fun origin -> walk actions network origin cond)
  |> List.fold ~init:1 ~f:lcm
  |> Int.to_string
;;

let%expect_test "AoC 2023: Day 08 - parse_input" =
  let input = Aoc.Common.Input.read_lines "test-1A.txt" in
  let instructions, nodes = parse_input input in
  Printf.printf "Part 1 (A): %s\n" (show_instructions instructions);
  Printf.printf "Part 1 (A): %s\n" (show_nodes nodes);
  let input = Aoc.Common.Input.read_lines "test-1B.txt" in
  let instructions, nodes = parse_input input in
  Printf.printf "Part 1 (B): %s\n" (show_instructions instructions);
  Printf.printf "Part 1 (B): %s\n" (show_nodes nodes);
  [%expect
    {|
    Part 1 (A): ['R'; 'L']
    Part 1 (A): [("AAA", "BBB", "CCC"); ("BBB", "DDD", "EEE"); ("CCC", "ZZZ", "GGG");
      ("DDD", "DDD", "DDD"); ("EEE", "EEE", "EEE"); ("GGG", "GGG", "GGG");
      ("ZZZ", "ZZZ", "ZZZ")]
    Part 1 (B): ['L'; 'L'; 'R']
    Part 1 (B): [("AAA", "BBB", "BBB"); ("BBB", "AAA", "ZZZ"); ("ZZZ", "ZZZ", "ZZZ")]
    |}]
;;

let%expect_test "AoC 2023: Day 08" =
  let input = Aoc.Common.Input.read_lines "test-1A.txt" in
  Printf.printf "Part 1 (A): %s\n" (solve_part_one input);
  [%expect {| Part 1 (A): 2 |}];
  let input = Aoc.Common.Input.read_lines "test-1B.txt" in
  Printf.printf "Part 1 (B): %s\n" (solve_part_one input);
  [%expect {| Part 1 (B): 6 |}];
  let input = Aoc.Common.Input.read_lines "test-2.txt" in
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: 6 |}]
;;
