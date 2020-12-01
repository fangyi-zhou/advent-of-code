open! Stdio
open Aoc

let () =
  let args = Sys.argv in
  let day = args.(1) in
  let input_file = Printf.sprintf "inputs/%s.in" day in
  let file = In_channel.create input_file in
  let inputs = In_channel.input_all file in
  let () =
    match day with "1" -> Day1.day1 inputs | _ -> failwith "Invalid day"
  in
  In_channel.close file
