open! Base
open! Stdio

module M = struct
  type t = Set.M(Int).t

  let parse inputs =
    let lines = String.split inputs ~on:'\n' in
    let nums = List.map ~f:Int.of_string lines in
    let nums = Set.of_list (module Int) nums in
    nums

  let sum2 sum nums =
    Set.fold_until ~init:()
      ~f:(fun () num ->
        if Set.mem nums (sum - num) then Stop (Some (num * (sum - num)))
        else Continue ())
      ~finish:(fun () -> None)
      nums

  let part1 nums =
    let ans = Option.value_exn (sum2 2020 nums) in
    print_endline (Int.to_string ans)

  let part2 nums =
    let ans =
      Set.fold_until ~init:nums
        ~f:(fun remaining num ->
          match sum2 (2020 - num) remaining with
          | Some answer -> Stop (answer * num)
          | None -> Continue (Set.remove nums num))
        ~finish:(fun _ -> assert false)
        nums
    in
    print_endline (Int.to_string ans)
end

include M
include Day.Make (M)

let example = "1721\n979\n366\n299\n675\n1456"

let%expect_test _ = run example ; [%expect {|
    514579
    241861950 |}]
