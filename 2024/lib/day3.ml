open! Imports
open Re

module M = struct
  type t = string

  let parse inputs = inputs

  let mul_regex_raw =
    Re.seq
      [ Re.str "mul("
      ; Re.group (Re.rep1 Re.digit)
      ; Re.str ","
      ; Re.group (Re.rep1 Re.digit)
      ; Re.str ")" ]

  let mul_regex = Re.compile mul_regex_raw

  let mul_regex_with_toggles_raw =
    Re.alt [mul_regex_raw; Re.str "do()"; Re.str "don't()"]

  let mul_regex_with_toggles = Re.compile mul_regex_with_toggles_raw

  let match_mult re str =
    let rec aux start acc =
      match Re.exec_opt ~pos:start re str with
      | Some group -> aux (Group.stop group 0) (group :: acc)
      | None -> List.rev acc
    in
    aux 0 []

  let part1 line =
    let matches = match_mult mul_regex line in
    let ans =
      List.sum
        (module Int)
        ~f:(fun group ->
          let left = Group.get group 1 in
          let right = Group.get group 2 in
          Int.of_string left * Int.of_string right )
        matches
    in
    print_endline_int ans

  let part2 line =
    let matches = match_mult mul_regex_with_toggles line in
    let ans, _ =
      List.fold ~init:(0, true)
        ~f:(fun (acc, enabled) group ->
          match Group.get group 0 with
          | "do()" -> (acc, true)
          | "don't()" -> (acc, false)
          | mul when String.is_prefix ~prefix:"mul(" mul ->
              if enabled then
                let left = Group.get group 1 in
                let right = Group.get group 2 in
                let value = Int.of_string left * Int.of_string right in
                (acc + value, enabled)
              else (acc, enabled)
          | _ -> assert false )
        matches
    in
    print_endline_int ans
end

include M
include Day.Make (M)

let example1 =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let%expect_test _ =
  run ~only_part1:true example1 ;
  [%expect {| 161 |}]

let example2 =
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let%expect_test _ =
  run ~only_part2:true example2 ;
  [%expect {| 48 |}]

let%expect_test _ =
  let group = Re.exec mul_regex "mul(2,3)" in
  Array.iter ~f:Stdio.print_endline (Group.all group) ;
  [%expect {|
    mul(2,3)
    2
    3
    |}]

let%expect_test _ =
  let groups = match_mult mul_regex "mul(2,3)mul(4,6)" in
  List.iter
    ~f:(fun group -> Array.iter ~f:Stdio.print_endline (Group.all group))
    groups ;
  [%expect {|
    mul(2,3)
    2
    3
    mul(4,6)
    4
    6
    |}]

let%expect_test _ =
  let groups =
    match_mult mul_regex_with_toggles "do()mul(2,3)mul(4,6)don't()"
  in
  List.iter
    ~f:(fun group -> Array.iter ~f:Stdio.print_endline (Group.all group))
    groups ;
  [%expect
    {|
    do()


    mul(2,3)
    2
    3
    mul(4,6)
    4
    6
    don't()
    |}]
