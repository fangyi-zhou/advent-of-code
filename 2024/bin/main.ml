open! Base
open! Stdio
open Aoc
open Utils

let () =
  let args = Sys.get_argv () in
  let day = args.(1) in
  let input_file = Printf.sprintf "inputs/%s.in" day in
  if not @@ Stdlib.Sys.file_exists input_file then
    download_input day input_file ;
  let file = In_channel.create input_file in
  let inputs = In_channel.input_all file in
  let (module Day : Day.S) =
    match day with "1" -> (module Day1) | _ -> failwith "invalid day"
  in
  Day.run inputs ; In_channel.close file
