open Core

type tile =
  | Vertical
  | Horizontal
  | NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  | Ground
  | Start
[@@deriving eq]

and field = tile array array [@@deriving show]

let as_tile (ch : char) : tile =
  match ch with
  | '|' -> Vertical
  | '-' -> Horizontal
  | 'L' -> NorthEast
  | 'J' -> NorthWest
  | 'F' -> SouthEast
  | '7' -> SouthWest
  | '.' -> Ground
  | 'S' -> Start
  | ch -> Fmt.failwith "invalid tile: %c" ch
;;

type indices = int * int [@@deriving show, eq, ord]
type offset = int * int [@@deriving show, eq, ord]
type neighbourhood = indices list [@@deriving show, eq, ord]

let parse_line (line : string) : tile array =
  line |> String.to_array |> Array.map ~f:as_tile
;;

let parse_input (input : string) : field =
  input |> String.split_lines |> List.map ~f:parse_line |> List.to_array
;;

let find_start (field : field) : int * int =
  field
  |> Array.find_mapi_exn ~f:(fun i row ->
    Array.find_mapi
      ~f:(fun j tile -> if equal_tile tile Start then Some (i, j) else None)
      row)
;;

let north : offset = -1, 0
let south : offset = 1, 0
let east : offset = 0, 1
let west : offset = 0, -1

let neighbourhood_offset (tile : tile) : offset list =
  match tile with
  | Vertical -> [ north; south ]
  | Horizontal -> [ west; east ]
  | NorthEast -> [ north; east ]
  | NorthWest -> [ north; west ]
  | SouthEast -> [ south; east ]
  | SouthWest -> [ south; west ]
  | Start -> [ north; south; west; east ]
  | Ground -> []
;;

let ( +> ) ((i, j) : indices) ((di, dj) : offset) : indices = i + di, j + dj

let get_tile (field : field) (indices : indices) =
  let i, j = indices in
  field.(i).(j)
;;

let validate rows cols (indices : indices) : indices option =
  match indices with
  | i, j when i < 0 || i >= rows || j < 0 || j >= cols -> None
  | i, j -> Some (i, j)
;;

let rec find_neighbours (field : field) (indices : indices) : neighbourhood =
  let rows = Array.length field in
  let cols = Array.length field.(0) in
  let tile = get_tile field indices in
  tile
  |> neighbourhood_offset
  |> List.map ~f:(fun offset -> indices +> offset)
  |> List.filter_map ~f:(validate rows cols)
  |> List.filter_map ~f:(infer_connections field indices)
  |> List.sort ~compare:compare_indices

and infer_connections (field : field) (indices : indices) (neighbour : indices)
  : indices option
  =
  match get_tile field indices with
  | Start ->
    if List.mem (find_neighbours field neighbour) indices ~equal:equal_indices
    then Some neighbour
    else None
  | _ -> Some neighbour
;;

let solve_part_one (_input : string) : string = failwith "TODO"
let solve_part_two (_input : string) : string = failwith "TODO"

let%expect_test "AoC 2023: Day 10 - parse_input" =
  let input = Aoc.Common.Input.read_lines "test-1A.txt" in
  Printf.printf "Part 1 (A):\n%s\n\n" (parse_input input |> show_field);
  [%expect
    {|
    Part 1 (A):
    [|[|Solution.Ground; Solution.Ground; Solution.Ground; Solution.Ground;
        Solution.Ground|];
      [|Solution.Ground; Solution.Start; Solution.Horizontal; Solution.SouthWest;
        Solution.Ground|];
      [|Solution.Ground; Solution.Vertical; Solution.Ground; Solution.Vertical;
        Solution.Ground|];
      [|Solution.Ground; Solution.NorthEast; Solution.Horizontal;
        Solution.NorthWest; Solution.Ground|];
      [|Solution.Ground; Solution.Ground; Solution.Ground; Solution.Ground;
        Solution.Ground|]
      |]
    |}];
  let input = Aoc.Common.Input.read_lines "test-1B.txt" in
  Printf.printf "Part 1 (B):\n%s\n\n" (parse_input input |> show_field);
  [%expect
    {|
    Part 1 (B):
    [|[|Solution.Ground; Solution.Ground; Solution.SouthEast; Solution.SouthWest;
        Solution.Ground|];
      [|Solution.Ground; Solution.SouthEast; Solution.NorthWest;
        Solution.Vertical; Solution.Ground|];
      [|Solution.Start; Solution.NorthWest; Solution.Ground; Solution.NorthEast;
        Solution.SouthWest|];
      [|Solution.Vertical; Solution.SouthEast; Solution.Horizontal;
        Solution.Horizontal; Solution.NorthWest|];
      [|Solution.NorthEast; Solution.NorthWest; Solution.Ground; Solution.Ground;
        Solution.Ground|]
      |]
    |}]
;;

let%expect_test "AoC 2023: Day 10 - find_start" =
  let input = Aoc.Common.Input.read_lines "test-1A.txt" in
  let field = parse_input input in
  let coords = find_start field in
  let tile = get_tile field coords in
  Printf.printf "Part 1 (A): [%s] %s\n" (show_tile tile) (coords |> show_indices);
  [%expect {| Part 1 (A): [Solution.Start] (1, 1) |}];
  let input = Aoc.Common.Input.read_lines "test-1B.txt" in
  let field = parse_input input in
  let coords = find_start field in
  let tile = get_tile field coords in
  Printf.printf "Part 1 (B): [%s] %s\n" (show_tile tile) (coords |> show_indices);
  [%expect {| Part 1 (B): [Solution.Start] (2, 0) |}];
  let input = Aoc.Common.Input.read_lines "test-1C.txt" in
  let field = parse_input input in
  let coords = find_start field in
  let tile = get_tile field coords in
  Printf.printf "Part 1 (C): [%s] %s\n" (show_tile tile) (coords |> show_indices);
  [%expect {| Part 1 (C): [Solution.Start] (1, 1) |}]
;;

let%expect_test "AoC 2023: Day 10 - find_neighbours" =
  let input = Aoc.Common.Input.read_lines "test-1A.txt" in
  let field = parse_input input in
  let coords = 1, 2 in
  let tile = get_tile field coords in
  Printf.printf
    "Part 1 (A): [%s] %s\n"
    (show_tile tile)
    (find_neighbours field coords |> show_neighbourhood);
  [%expect {| Part 1 (A): [Solution.Horizontal] [(1, 1); (1, 3)] |}];
  let coords = 3, 3 in
  let tile = get_tile field coords in
  Printf.printf
    "Part 1 (A): [%s] %s\n"
    (show_tile tile)
    (find_neighbours field coords |> show_neighbourhood);
  [%expect {| Part 1 (A): [Solution.NorthWest] [(2, 3); (3, 2)] |}];
  let coords = find_start field in
  let tile = get_tile field coords in
  Printf.printf
    "Part 1 (A): [%s] %s\n"
    (show_tile tile)
    (find_neighbours field coords |> show_neighbourhood);
  [%expect {| Part 1 (A): [Solution.Start] [(1, 2); (2, 1)] |}];
  let input = Aoc.Common.Input.read_lines "test-1B.txt" in
  let field = parse_input input in
  let coords = find_start field in
  let tile = get_tile field coords in
  Printf.printf
    "Part 1 (B): [%s] %s\n"
    (show_tile tile)
    (find_neighbours field coords |> show_neighbourhood);
  [%expect {| Part 1 (B): [Solution.Start] [(2, 1); (3, 0)] |}];
  let input = Aoc.Common.Input.read_lines "test-1C.txt" in
  let field = parse_input input in
  let coords = find_start field in
  let tile = get_tile field coords in
  Printf.printf
    "Part 1 (C): [%s] %s\n"
    (show_tile tile)
    (find_neighbours field coords |> show_neighbourhood);
  [%expect {| Part 1 (C): [Solution.Start] [(1, 2); (2, 1)] |}]
;;

(* let%expect_test "AoC 2023: Day 10" =
  let input = Aoc.Common.Input.read_lines "test-1A.txt" in
  Printf.printf "Part 1 (A): %s\n" (solve_part_one input);
  [%expect {| Part 1 (A): 10 |}];
  let input = Aoc.Common.Input.read_lines "test-1B.txt" in
  Printf.printf "Part 1 (B): %s\n" (solve_part_one input);
  [%expect {| Part 1 (B): 10 |}];
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: |}]
;; *)
