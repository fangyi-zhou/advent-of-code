open Imports

module M = struct
  type t = int * int

  let modular = 20201227

  let subj = 7

  let find_loop_size target =
    let rec aux loop acc =
      if acc = target then loop else aux (loop + 1) (acc * subj % modular)
    in
    aux 0 1

  let rec pow init times =
    if times = 0 then 1
    else if times % 2 = 0 then
      let half = pow init (times / 2) in
      half * half % modular
    else
      let rest = pow init (times - 1) in
      init * rest % modular

  let parse inputs =
    let lines = String.split inputs ~on:'\n' in
    match List.map ~f:Int.of_string lines with
    | [num1; num2] -> (num1, num2)
    | _ -> assert false

  let part1 (n1, n2) =
    let _l1 = find_loop_size n1 in
    let l2 = find_loop_size n2 in
    let ans = pow n1 l2 in
    print_endline_int ans

  let part2 _ = ()
end

include M
include Day.Make (M)

let example = "5764801\n17807724"

let%expect_test _ = run example ; [%expect {| 14897079 |}]
