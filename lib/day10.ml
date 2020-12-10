open Imports

module M = struct
  type t = int list

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    List.map ~f:Int.of_string lines

  let part1 devices =
    let sorted = List.sort ~compare devices in
    let diff1, diff3, _ =
      List.fold ~init:(0, 1, 0) (* always have +3 in the end *)
        ~f:(fun (diff1, diff3, last) curr ->
          if curr = last + 1 then (diff1 + 1, diff3, curr)
          else if curr = last + 3 then (diff1, diff3 + 1, curr)
          else (diff1, diff3, curr))
        sorted
    in
    let ans = diff1 * diff3 in
    print_endline_int ans

  let part2 _ = ()
end

include Day.Make (M)

let example1 = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"

let example2 =
  "28\n\
   33\n\
   18\n\
   42\n\
   31\n\
   14\n\
   46\n\
   20\n\
   48\n\
   47\n\
   24\n\
   23\n\
   49\n\
   45\n\
   19\n\
   38\n\
   39\n\
   11\n\
   1\n\
   32\n\
   25\n\
   35\n\
   8\n\
   17\n\
   7\n\
   9\n\
   4\n\
   2\n\
   34\n\
   10\n\
   3"

let%expect_test _ = run example1 ; [%expect {| 35 |}]

let%expect_test _ = run example2 ; [%expect {| 220 |}]
