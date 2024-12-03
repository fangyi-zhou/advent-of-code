open! Imports

module M = struct
  type t = int List.t List.t

  let parse inputs =
    let lines = String.split_lines inputs in
    let nums =
      List.map
        ~f:(fun line -> List.map ~f:Int.of_string (String.split ~on:' ' line))
        lines
    in
    nums

  let is_line_safe line =
    List.fold_until ~init:(`Init, None)
      ~f:(fun (trend, prev) curr ->
        match (trend, prev) with
        | `Init, None -> Continue (`Init, Some curr)
        | `Init, Some prev when prev = curr -> Stop false
        | `Init, Some prev when prev > curr && Int.abs (prev - curr) <= 3 ->
            Continue (`Incr, Some curr)
        | `Init, Some prev when prev < curr && Int.abs (prev - curr) <= 3 ->
            Continue (`Decr, Some curr)
        | `Init, Some _ -> Stop false
        | `Incr, Some prev when prev > curr && Int.abs (prev - curr) <= 3 ->
            Continue (`Incr, Some curr)
        | `Incr, Some _ -> Stop false
        | `Decr, Some prev when prev < curr && Int.abs (prev - curr) <= 3 ->
            Continue (`Decr, Some curr)
        | `Decr, Some _ -> Stop false
        | `Incr, None -> assert false
        | `Decr, None -> assert false )
      ~finish:(fun _ -> true)
      line

  let remove_item xs =
    let rec aux xs prev acc =
      match xs with
      | [] -> acc
      | x :: rest ->
          let acc = (List.rev prev @ rest) :: acc in
          aux rest (x :: prev) acc
    in
    aux xs [] []

  let part1 lines =
    let ans = List.count ~f:is_line_safe lines in
    print_endline_int ans

  let part2 lines =
    let ans =
      List.count
        ~f:(fun line ->
          List.exists ~f:is_line_safe (line :: remove_item line) )
        lines
    in
    print_endline_int ans
end

include M
include Day.Make (M)

let example =
  "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

let%expect_test _ =
  run example ;
  [%expect
    {|
                                    2
                                    4
                                    |}]

let%expect_test _ =
  let removed = remove_item [1; 2; 3; 4; 5] in
  List.iter
    ~f:(fun xs ->
      Stdio.print_endline
        (String.concat ~sep:", " (List.map ~f:Int.to_string xs)) )
    removed ;
  [%expect
    {|
    1, 2, 3, 4
    1, 2, 3, 5
    1, 2, 4, 5
    1, 3, 4, 5
    2, 3, 4, 5
    |}]
