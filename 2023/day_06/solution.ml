open Core
open Angstrom

let sep = take_while (fun c -> Char.equal c ' ' || Char.equal c '\t')
let digits = take_while1 Char.is_digit
let label = take_while1 Char.is_alpha
let line = label *> string ":" *> sep *> sep_by1 sep digits

let races =
  let* times = line <* end_of_line >>| List.map ~f:Int.of_string in
  let* distances = line <* end_of_input >>| List.map ~f:Int.of_string in
  return (times, distances)
;;

let single_race =
  let* time = line <* end_of_line >>| String.concat ~sep:"" >>| Int.of_string in
  let* distance =
    line <* end_of_input >>| String.concat ~sep:"" >>| Int.of_string
  in
  return (time, distance)
;;

let parse_input (parser : 'a Angstrom.t) (input : string) =
  match parse_string ~consume:All parser input with
  | Ok v -> v
  | Error err -> failwith err
;;

let record_beaten (time : int) (distance : int) (hold : int) : bool =
  let speed = hold in
  let remaining = time - hold in
  speed * remaining > distance
;;

let record_beaten_ways (time : int) (distance : int) : int =
  List.init time ~f:Fun.id
  |> List.filter ~f:(record_beaten time distance)
  |> List.length
;;

let thresholds (time : int) (distance : int) : (int * int) option =
  (* 
    hold * (time - hold) > distance 
    => - hold^2 + time * hold - distance > 0
    => -time +- sqrt (time^2 - 4 distance)
  *)
  let a = 1.0 in
  let b = -1.0 *. Float.of_int time in
  let c = Float.of_int distance in
  let delta = (b ** 2.0) -. (4.0 *. a *. c) in
  if Float.compare delta 0.0 < 0
  then None
  else (
    let hold_min = (-.b -. (delta ** 0.5)) /. (2.0 *. a) in
    let hold_max = (-.b +. (delta ** 0.5)) /. (2.0 *. a) in
    match List.sort ~compare:Float.compare [ hold_min; hold_max ] with
    | [ hold_min; hold_max ] ->
      let hold_min = Float.round_down hold_min |> Int.of_float |> Int.succ in
      let hold_max = Float.round_up hold_max |> Int.of_float |> Int.pred in
      Some (hold_min, hold_max)
    | _ -> failwith "thresholds: unreachable")
;;

let solve_part_one (input : string) : string =
  let times, distances = parse_input races input in
  List.map2_exn times distances ~f:record_beaten_ways
  |> List.fold ~init:1 ~f:( * )
  |> Int.to_string
;;

let solve_part_two (input : string) : string =
  let time, distance = parse_input single_race input in
  let result =
    match thresholds time distance with
    | Some (hold_min, hold_max) -> hold_max - hold_min + 1
    | None -> 0
  in
  Int.to_string result
;;

let%expect_test "AoC 2023: Day 06" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  Printf.printf "Part 1: %s\n" (solve_part_one input);
  [%expect {| Part 1: 288 |}];
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: 71503 |}]
;;
