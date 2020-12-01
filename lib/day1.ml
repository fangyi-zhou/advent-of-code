open! Base
open! Stdio

let day1 inputs =
  let lines = String.split inputs ~on:'\n' in
  let nums = List.map ~f:Int.of_string lines in
  let nums = Set.of_list (module Int) nums in
  let answer =
    Set.fold_until ~init:()
      ~f:(fun () num ->
        if Set.mem nums (2020 - num) then Stop (num * (2020 - num))
        else Continue ())
      ~finish:(fun () -> assert false)
      nums
  in
  print_endline (Int.to_string answer)
