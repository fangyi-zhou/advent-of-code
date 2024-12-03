open! Imports

module M = struct
  type t = string list

  let parse inputs =
    let lines = String.split_lines inputs in
    lines

  let part1 _ = ()

  let part2 _ = ()
end

include M
include Day.Make (M)

let example = ""

let%expect_test _ = run example ; [%expect {| |}]
