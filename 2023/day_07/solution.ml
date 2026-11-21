open Core

let rank (label : char) : int =
  match label with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | '9' -> 9
  | '8' -> 8
  | '7' -> 7
  | '6' -> 6
  | '5' -> 5
  | '4' -> 4
  | '3' -> 3
  | '2' -> 2
  | ch -> failwith (Printf.sprintf "invalid rank: %c" ch)
;;

type hand = char list [@@deriving show, ord]
type counts = int list [@@deriving show]

type kind =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FourOfAKind
  | FiveOfAKind
[@@deriving ord]

let evaluate ?(joker = false) (hand : hand) : kind =
  let counter =
    hand
    |> List.fold
         ~f:(fun acc ch ->
           Map.update acc ch ~f:(fun key ->
             Option.map key ~f:Int.succ |> Option.value ~default:1))
         ~init:Char.Map.empty
  in
  let joker_count =
    if joker then Map.find counter 'J' |> Option.value ~default:0 else 0
  in
  let counter = if joker then Map.remove counter 'J' else counter in
  let counts =
    Map.keys counter
    |> List.filter_map ~f:(Map.find counter)
    |> List.sort ~compare:(Fun.flip Int.compare)
  in
  let counts =
    match counts with
    | [] -> [ joker_count ]
    | curr :: rest -> (curr + joker_count) :: rest
  in
  match counts with
  | [ 5 ] -> FiveOfAKind
  | 4 :: _ -> FourOfAKind
  | 3 :: _ -> ThreeOfAKind
  | 2 :: 2 :: _ -> TwoPair
  | 2 :: 1 :: _ -> OnePair
  | 1 :: _ -> HighCard
  | _ -> Fmt.failwith "invalid hand : %a@." pp_counts counts
;;

let compare_hand ?(joker = false) fst_hand snd_hand =
  match compare_kind (evaluate ~joker fst_hand) (evaluate ~joker snd_hand) with
  | 0 ->
    List.compare (fun a b -> Int.compare (rank a) (rank b)) fst_hand snd_hand
  | c -> c
;;

let parse_line (line : string) : hand * int =
  match String.split line ~on:' ' with
  | [ hand; bid ] ->
    let hand = hand |> String.to_list in
    let bid = bid |> Int.of_string in
    hand, bid
  | _ -> failwith (Printf.sprintf "invalid line: %s" line)
;;

let solve_part_one (input : string) : string =
  input
  |> String.split_lines
  |> List.map ~f:parse_line
  |> List.sort ~compare:(fun (fst_hand, _) (snd_hand, _) ->
    compare_hand fst_hand snd_hand)
  |> List.mapi ~f:(fun i (_, bid) -> bid * (i + 1))
  |> List.sum (module Int) ~f:Fun.id
  |> Int.to_string
;;

let solve_part_two (input : string) : string =
  input
  |> String.split_lines
  |> List.map ~f:parse_line
  |> List.sort ~compare:(fun (fst_hand, _) (snd_hand, _) ->
    compare_hand ~joker:true fst_hand snd_hand)
  |> List.mapi ~f:(fun i (_, bid) -> bid * (i + 1))
  |> List.sum (module Int) ~f:Fun.id
  |> Int.to_string
;;

let%expect_test "AoC 2023: Day 07" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  Printf.printf "Part 1: %s\n" (solve_part_one input);
  [%expect {| Part 1: 6440 |}];
  Printf.printf "Part 2: %s\n" (solve_part_two input);
  [%expect {| Part 2: 5905 |}]
;;
