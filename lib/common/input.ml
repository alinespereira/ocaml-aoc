open Base
open Stdlib

let read_lines filename =
  let channel = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
       let rec loop lines =
         match input_line channel with
         | line -> loop (line :: lines)
         | exception End_of_file -> List.rev lines |> String.concat "\n"
       in
       loop [])
;;

let split_lines = String.split_on_char '\n'
