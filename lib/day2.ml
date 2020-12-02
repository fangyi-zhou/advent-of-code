open! Base
open! Stdio

let parse line =
  let splitted = String.split_on_chars ~on:[':'; ' '; '-'] line in
  match splitted with
  | mini :: maxi :: letter :: _ :: pwd :: _ ->
      (Int.of_string mini, Int.of_string maxi, letter.[0], pwd)
  | _ -> assert false

let part1 entries =
  List.count
    ~f:(fun (mini, maxi, letter, pwd) ->
      let count = String.count ~f:(Char.( = ) letter) pwd in
      count >= mini && count <= maxi)
    entries

let part2 entries =
  List.count
    ~f:(fun (pos1, pos2, letter, pwd) ->
      not
      @@ Bool.equal
           (Char.equal pwd.[pos1 - 1] letter)
           (Char.equal pwd.[pos2 - 1] letter))
    entries

let day2 inputs =
  let lines = String.split inputs ~on:'\n' in
  let parsed = List.map ~f:parse lines in
  let answer1 = part1 parsed in
  let answer2 = part2 parsed in
  print_endline (Int.to_string answer1) ;
  print_endline (Int.to_string answer2)

let example = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"

let%expect_test _ = day2 example ; [%expect {|
    2
    1 |}]
