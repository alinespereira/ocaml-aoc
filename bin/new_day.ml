let fail message =
  prerr_endline ("Erro: " ^ message);
  exit 1
;;

let rec find_project_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project")
  then dir
  else (
    let parent = Filename.dirname dir in
    if parent = dir
    then fail "não encontrei dune-project"
    else find_project_root parent)
;;

let rec ensure_directory path =
  if Sys.file_exists path
  then (
    if not (Sys.is_directory path)
    then fail (path ^ " existe, mas não é uma pasta"))
  else (
    ensure_directory (Filename.dirname path);
    Unix.mkdir path 0o755)
;;

let write_file path contents =
  let channel =
    open_out_gen [ Open_wronly; Open_creat; Open_excl; Open_text ] 0o644 path
  in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel contents)
;;

(* Aplica o .ocamlformat do projeto aos arquivos gerados. *)
let format_files paths =
  let command =
    String.concat " " ("ocamlformat --inplace" :: List.map Filename.quote paths)
  in
  if Sys.command command <> 0 then prerr_endline "Aviso: ocamlformat falhou"
;;

let parse_number label value =
  match int_of_string_opt value with
  | Some number -> number
  | None -> fail (label ^ " precisa ser um número")
;;

let () =
  if Array.length Sys.argv <> 3
  then fail "uso: dune exec bin/new_day.exe -- ANO DIA";
  let year = parse_number "ano" Sys.argv.(1) in
  let day = parse_number "dia" Sys.argv.(2) in
  if year < 2015 then fail "o Advent of Code começou em 2015";
  if day < 1 || day > 25 then fail "o dia deve estar entre 1 e 25";
  let root = find_project_root (Sys.getcwd ()) in
  let day_name = Printf.sprintf "day_%02d" day in
  let relative_dir = Printf.sprintf "%d/%s" year day_name in
  let target = Filename.concat root relative_dir in
  if Sys.file_exists target then fail (relative_dir ^ " já existe");
  ensure_directory target;
  let dune_file =
    Printf.sprintf
      {|(library
 (name aoc%d%02d)
 (modules solution)
 (libraries aoc fmt angstrom)
 (inline_tests
  (deps test.txt))
 (preprocess
  (pps ppx_expect)))

(executable
 (name main)
 (modules main)
 (libraries aoc%d%02d))
|}
      year
      day
      year
      day
  in
  write_file (Filename.concat target "dune") dune_file;
  let solution =
    Printf.sprintf
      {ocaml|let solve_part_one (_input : string) : string =
  failwith "TODO"

let solve_part_two (_input : string) : string =
  failwith "TODO"

let%%expect_test "AoC %d: Day %02d" =
  let input = Aoc.Common.Input.read_lines "test.txt" in
  Printf.printf "Part 1: %%s\n" (solve_part_one input);
  [%%expect {| Part 1: |}];
  Printf.printf "Part 2: %%s\n" (solve_part_two input);
  [%%expect {| Part 2: |}]
;;
|ocaml}
      year
      day
  in
  let solution_path = Filename.concat target "solution.ml" in
  write_file solution_path solution;
  let input_path = relative_dir ^ "/input.txt" in
  let main =
    Printf.sprintf
      {|let () =
  let input =
    Aoc.Common.Input.read_lines "%s"
  in
  Printf.printf "Part 1: %%s\n" (Aoc%d%02d.Solution.solve_part_one input);
  Printf.printf "Part 2: %%s\n" (Aoc%d%02d.Solution.solve_part_two input)
;;
|}
      input_path
      year
      day
      year
      day
  in
  let main_path = Filename.concat target "main.ml" in
  write_file main_path main;
  write_file (Filename.concat target "input.txt") "";
  write_file (Filename.concat target "test.txt") "";
  format_files [ solution_path; main_path ];
  Printf.printf "Criado: %s\n" relative_dir
;;
