open! Base
open! Stdio

module M = struct
  type t = string list list

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let f (acc, curr) line =
      if String.is_empty line then (curr :: acc, []) else (acc, line :: curr)
    in
    let acc, last = List.fold ~init:([], []) ~f lines in
    List.rev (last :: acc)

  let char_set_of_string str =
    String.fold ~init:(Set.empty (module Char)) ~f:Set.add str

  let process set_op entries =
    let f entry =
      let string_sets = List.map ~f:char_set_of_string entry in
      set_op string_sets
    in
    let sets = List.map ~f entries in
    let ans = List.sum (module Int) ~f:Set.length sets in
    print_endline (Int.to_string ans)

  let inter_list = function
    | [] -> assert false
    | hd :: tl -> List.fold ~init:hd ~f:Set.inter tl

  let part1 = process @@ Set.union_list (module Char)

  let part2 = process @@ inter_list
end

include Day.Make (M)

let example = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"

let%expect_test _ = run example ; [%expect {|
  11
  6|}]
