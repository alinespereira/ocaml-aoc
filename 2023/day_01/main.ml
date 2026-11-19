let () =
  let input =
    Aoc.Common.Input.read_lines "2023/day_01/input.txt" |> String.concat "\n"
  in

  Printf.printf "Part 1: %s\n"
    (Aoc202301.Solution.solve_part_one input);

  Printf.printf "Part 2: %s\n"
    (Aoc202301.Solution.solve_part_two input)
