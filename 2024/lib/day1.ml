open! Imports

module M = struct
  type t = {left: int List.t; right: int List.t}

  let parse inputs =
    let lines = String.split_lines inputs in
    let nums =
      List.map
        ~f:(fun line ->
          match
            List.filter_map
              ~f:(fun s ->
                if String.is_empty s then None else Some (Int.of_string s) )
              (String.split line ~on:' ')
          with
          | [left; right] -> (left, right)
          | _ -> assert false )
        lines
    in
    {left= List.map ~f:fst nums; right= List.map ~f:snd nums}

  let part1 {left; right} =
    let left = List.sort ~compare:Int.compare left in
    let right = List.sort ~compare:Int.compare right in
    let ans =
      List.sum
        (module Int)
        (List.rev_map2_exn ~f:(fun x y -> Int.abs (x - y)) left right)
        ~f:Fn.id
    in
    print_endline_int ans

  let make_counter comp lst =
    let init = Map.empty comp in
    List.fold_left ~init
      ~f:(fun acc x ->
        Map.update acc x ~f:(fun count -> Option.value ~default:0 count + 1) )
      lst

  let part2 {left; right} =
    let right_counter = make_counter (module Int) right in
    let ans =
      List.sum
        (module Int)
        left
        ~f:(fun num ->
          num * Option.value ~default:0 (Map.find right_counter num) )
    in
    print_endline_int ans
end

include M
include Day.Make (M)

let example = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

let%expect_test _ = run example ; [%expect {|
                                    11
                                    31
                                    |}]
