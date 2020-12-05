open! Stdio
open Aoc

let () =
  let args = Sys.argv in
  let day = args.(1) in
  let input_file = Printf.sprintf "inputs/%s.in" day in
  let file = In_channel.create input_file in
  let inputs = In_channel.input_all file in
  let (module Day : Day.S) =
    match day with
    | "1" -> (module Day1)
    | "2" -> (module Day2)
    | "3" -> (module Day3)
    | "4" -> (module Day4)
    | "5" -> (module Day5)
    | _ -> failwith "invalid day"
  in
  Day.run inputs ; In_channel.close file
