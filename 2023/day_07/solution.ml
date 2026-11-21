let solve_part_one (_input : string) : string = failwith "TODO"
let solve_part_two (_input : string) : string = failwith "TODO"

let%expect_test "AoC 2023: Day 07" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  Printf.printf "Part 1: %s\n" (solve_part_one input);
  [%expect {| Part 1: |}];
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: |}]
;;
