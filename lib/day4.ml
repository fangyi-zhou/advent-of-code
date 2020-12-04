open! Base
open! Stdio

type passport =
  { byr: string option
  ; iyr: string option
  ; eyr: string option
  ; hgt: string option
  ; hcl: string option
  ; ecl: string option
  ; pid: string option
  ; cid: string option }

let new_passport =
  { byr= None
  ; iyr= None
  ; eyr= None
  ; hgt= None
  ; hcl= None
  ; ecl= None
  ; pid= None
  ; cid= None }

let process_passport (acc, curr) line =
  if String.is_empty line then (curr :: acc, new_passport)
  else
    let entries = String.split ~on:' ' line in
    let f passport entry =
      match String.split ~on:':' entry with
      | [k; v] -> (
        match k with
        | "byr" -> {passport with byr= Some v}
        | "iyr" -> {passport with iyr= Some v}
        | "eyr" -> {passport with eyr= Some v}
        | "hgt" -> {passport with hgt= Some v}
        | "hcl" -> {passport with hcl= Some v}
        | "ecl" -> {passport with ecl= Some v}
        | "pid" -> {passport with pid= Some v}
        | "cid" -> {passport with cid= Some v}
        | _ -> assert false )
      | _ -> assert false
    in
    let curr = List.fold ~init:curr ~f entries in
    (acc, curr)

let part1 data =
  let f passport =
    Option.is_some passport.byr
    && Option.is_some passport.iyr
    && Option.is_some passport.eyr
    && Option.is_some passport.hgt
    && Option.is_some passport.hcl
    && Option.is_some passport.ecl
    && Option.is_some passport.pid
  in
  List.count ~f data

let day4 inputs =
  let lines = String.split ~on:'\n' inputs in
  let data, last =
    List.fold ~init:([], new_passport) ~f:process_passport lines
  in
  let data = List.rev (last :: data) in
  let answer1 = part1 data in
  print_endline (Int.to_string answer1)

let example =
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
   byr:1937 iyr:2017 cid:147 hgt:183cm\n\n\
   iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
   hcl:#cfa07d byr:1929\n\n\
   hcl:#ae17e1 iyr:2013\n\
   eyr:2024\n\
   ecl:brn pid:760753108 byr:1931\n\
   hgt:179cm\n\n\
   hcl:#cfa07d eyr:2025 pid:166559648\n\
   iyr:2011 ecl:brn hgt:59in"

let%expect_test _ = day4 example ; [%expect {|2|}]
