let () =
  let input = Aoc.Common.Input.read_lines "2023/day_08/input.txt" in
  Printf.printf "Part 1: %s\n" (Aoc202308.Solution.solve_part_one input);
  Printf.printf "Part 2: %s\n" (Aoc202308.Solution.solve_part_two input)
;;
