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

let validate_passport byr iyr eyr hgt hcl ecl pid cid passport =
  byr passport.byr && iyr passport.iyr && eyr passport.eyr
  && hgt passport.hgt && hcl passport.hcl && ecl passport.ecl
  && pid passport.pid && cid passport.cid

let part1 data =
  let f =
    validate_passport Option.is_some Option.is_some Option.is_some
      Option.is_some Option.is_some Option.is_some Option.is_some (fun _ ->
        true)
  in
  List.count ~f data

let part2 data =
  let num_range_suffix mini maxi suffix str_opt =
    try
      let str = Option.value_exn str_opt in
      let str = String.chop_suffix_exn str ~suffix in
      let num = Int.of_string str in
      num >= mini && num <= maxi
    with _ -> false
  in
  let num_range mini maxi str = num_range_suffix mini maxi "" str in
  let hair_color str_opt =
    match str_opt with
    | Some str ->
        let is_hex ch =
          Char.((ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f'))
        in
        String.length str = 7
        && Char.equal str.[0] '#'
        && List.for_all [1; 2; 3; 4; 5; 6] ~f:(fun idx -> is_hex str.[idx])
    | None -> false
  in
  let eye_color = function
    | Some ("amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth") -> true
    | _ -> false
  in
  let passport_id = function
    | Some str ->
        String.length str = 9
        && String.for_all ~f:(fun ch -> Char.(ch >= '0' && ch <= '9')) str
    | None -> false
  in
  let f =
    validate_passport (num_range 1920 2002) (num_range 2010 2020)
      (num_range 2020 2030)
      (fun s ->
        num_range_suffix 150 193 "cm" s || num_range_suffix 59 76 "in" s)
      hair_color eye_color passport_id
      (fun _ -> true)
  in
  List.count ~f data

let day4 ?only_part1 ?only_part2 inputs =
  let lines = String.split ~on:'\n' inputs in
  let data, last =
    List.fold ~init:([], new_passport) ~f:process_passport lines
  in
  let data = List.rev (last :: data) in
  let () =
    if Option.is_none only_part2 then
      let answer1 = part1 data in
      print_endline (Int.to_string answer1)
  in
  let () =
    if Option.is_none only_part1 then
      let answer2 = part2 data in
      print_endline (Int.to_string answer2)
  in
  ()

let example_1 =
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

let example_2 =
  "eyr:1972 cid:100\n\
   hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\n\
   iyr:2019\n\
   hcl:#602927 eyr:1967 hgt:170cm\n\
   ecl:grn pid:012533040 byr:1946\n\n\
   hcl:dab227 iyr:2012\n\
   ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\n\
   hgt:59cm ecl:zzz\n\
   eyr:2038 hcl:74454a iyr:2023\n\
   pid:3556412378 byr:2007"

let example_3 =
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
   hcl:#623a2f\n\n\
   eyr:2029 ecl:blu cid:129 byr:1989\n\
   iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\n\
   hcl:#888785\n\
   hgt:164cm byr:2001 iyr:2015 cid:88\n\
   pid:545766238 ecl:hzl\n\
   eyr:2022\n\n\
   iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"

let%expect_test _ =
  day4 ~only_part1:true example_1 ;
  [%expect {|2|}]

let%expect_test _ =
  day4 ~only_part2:true example_2 ;
  [%expect {|0|}]

let%expect_test _ =
  day4 ~only_part2:true example_3 ;
  [%expect {|4|}]
