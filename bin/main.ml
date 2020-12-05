open! Base
open! Stdio
open Aoc

let session_file = ".session"

let get_token () =
  let file = In_channel.create session_file in
  let token = In_channel.input_all file in
  In_channel.close file ; String.strip token

let download_input day fn =
  let open Unix in
  let open Lwt in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let year =
    let time = time () in
    let time = gmtime time in
    time.tm_year + 1900
  in
  let url =
    Printf.sprintf "https://adventofcode.com/%d/day/%s/input" year day
  in
  let token = get_token () in
  let headers = Header.init () in
  let headers = Header.add headers "Cookie" ("session=" ^ token) in
  let uri = Uri.of_string url in
  let body =
    Client.get ~headers uri
    >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    if code = 200 then body |> Cohttp_lwt.Body.to_string
    else failwith ("Unable to get data, status " ^ Int.to_string code)
  in
  let body = Lwt_main.run body in
  let body = String.strip body in
  let file = Out_channel.create fn in
  Out_channel.output_string file body ;
  Out_channel.close file

let () =
  let args = Sys.get_argv () in
  let day = args.(1) in
  let input_file = Printf.sprintf "inputs/%s.in" day in
  if not @@ Caml.Sys.file_exists input_file then
    download_input day input_file ;
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
